use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::{hash_map, HashMap, HashSet},
    fmt::Debug,
    iter::repeat_with,
    vec,
};

use crate::{
    traits::{
        lattice::{Bottom, LocalMinimum, Meet, ThreeWayCompare, Top, Union},
        structural::{Cons, InterpretBuiltin, TypeDomain, Uncons},
    },
    type_inference::Program,
    util::tup::Tup,
};

use super::{
    const_model::{NemoBuiltin, NemoCtor, NemoFunctor, NemoModel, NestedFunctor},
    flat_type::FlatType,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructuredType {
    start: TypeNode,
    grammar: TypeGrammar,
}

#[derive(Clone, PartialEq, Eq)]
enum TypeNode {
    Any,
    TypeNode(OrNode),
}

impl Debug for TypeNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Any => write!(f, "Any"),
            Self::TypeNode(arg0) => write!(f, "{:?}", arg0),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
struct OrNode {
    flat_types: FlatType,
    functors: HashSet<NestedFunctor>,
}

impl Debug for OrNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set()
            .entry(&self.flat_types)
            .entries(&self.functors)
            .finish()
    }
}

impl PartialOrd for OrNode {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let flat_ordering = self.flat_types.partial_cmp(&other.flat_types)?;

        // a subset b <=> (a \\ b) == {}
        let self_le = || self.functors.difference(&other.functors).next().is_none();
        let self_ge = || other.functors.difference(&self.functors).next().is_none();

        match flat_ordering {
            Ordering::Less => self_le().then_some(Ordering::Less),
            Ordering::Equal => match (self_le(), self_ge()) {
                (true, true) => Some(Ordering::Equal),
                (true, false) => Some(Ordering::Less),
                (false, true) => Some(Ordering::Greater),
                (false, false) => None,
            },
            Ordering::Greater => self_ge().then_some(Ordering::Greater),
        }
    }
}

impl Meet for OrNode {
    fn meet_with(&mut self, other: &Self) {
        self.flat_types.meet_with(&other.flat_types);
        self.functors.retain(|f| other.functors.contains(f))
    }
}

impl PartialOrd for TypeNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (TypeNode::Any, TypeNode::Any) => Some(Ordering::Equal),
            (TypeNode::Any, TypeNode::TypeNode { .. }) => Some(Ordering::Greater),
            (TypeNode::TypeNode { .. }, TypeNode::Any) => Some(Ordering::Less),
            (TypeNode::TypeNode(left), TypeNode::TypeNode(right)) => left.partial_cmp(right),
        }
    }
}

impl Meet for TypeNode {
    fn meet_with(&mut self, other: &Self) {
        let TypeNode::TypeNode(inner) = self else {
            *self = other.clone();
            return;
        };

        let TypeNode::TypeNode(other) = other else {
            return;
        };

        inner.flat_types.meet_with(&other.flat_types);
        inner.functors.retain(|f| other.functors.contains(f))
    }
}

impl TypeNode {
    fn functors(&self) -> Cow<'_, HashSet<NestedFunctor>> {
        match self {
            TypeNode::Any => Cow::Owned(HashSet::new()),
            TypeNode::TypeNode(OrNode { functors, .. }) => Cow::Borrowed(functors),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
struct TypeGrammar {
    rules: HashMap<NestedFunctor, Vec<TypeNode>>,
}

impl Debug for TypeGrammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;

        for (key, value) in &self.rules {
            write!(f, "{key:?} => {value:?}, ")?;
        }

        write!(f, "}}")
    }
}

impl TypeGrammar {
    fn new() -> Self {
        Self {
            rules: HashMap::new(),
        }
    }

    fn get(&self, functor: &NestedFunctor) -> Option<&[TypeNode]> {
        self.rules.get(functor).map(Vec::as_slice)
    }

    fn sub_grammar<'a>(&self, funcs: impl IntoIterator<Item = &'a NestedFunctor>) -> Self {
        let mut visit = Vec::from_iter(funcs);
        let mut rules = HashMap::new();

        while let Some(current) = visit.pop() {
            let hash_map::Entry::Vacant(entry) = rules.entry(current.clone()) else {
                continue;
            };

            let rhs = self
                .rules
                .get(current)
                .expect("grammar contains description for all transitive functors");

            entry.insert(rhs.clone());

            for node in rhs {
                let TypeNode::TypeNode(inner) = node else {
                    continue;
                };

                visit.extend(&inner.functors)
            }
        }

        Self { rules }
    }

    fn add_rule(&mut self, func: NestedFunctor, args: Vec<TypeNode>) {
        let mut entry = match self.rules.entry(func) {
            hash_map::Entry::Vacant(entry) => {
                entry.insert(args);
                return;
            }
            hash_map::Entry::Occupied(entry) => entry,
        };

        for (current, new) in entry.get_mut().iter_mut().zip(args) {
            let TypeNode::TypeNode(inner) = current else {
                continue;
            };

            let TypeNode::TypeNode(new) = new else {
                *current = TypeNode::Any;
                continue;
            };

            inner.flat_types.union_with(&new.flat_types);
            inner.functors.extend(new.functors);
        }
    }

    fn union_with(&mut self, other: TypeGrammar) {
        for (func, args) in other.rules {
            self.add_rule(func, args)
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructuredTypeConfig {}

impl PartialOrd for StructuredType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let mut comparison = ThreeWayCompare::init().chain(&self.start, &other.start)?;

        let mut visit: Vec<_> = self
            .start
            .functors()
            .intersection(&other.start.functors())
            .cloned()
            .collect();
        let mut visited = HashSet::new();

        while let Some(func) = visit.pop() {
            if !visited.insert(func.clone()) {
                continue;
            }

            let left = self
                .grammar
                .get(&func)
                .expect("common functor in both types");
            let right = other
                .grammar
                .get(&func)
                .expect("common functor in both types");

            for (left_node, right_node) in left.iter().zip(right) {
                comparison = comparison.chain(left_node, right_node)?;

                let left_functors = left_node.functors();
                let right_functors = right_node.functors();
                visit.extend(left_functors.intersection(&right_functors).cloned())
            }
        }

        Some(comparison.finish())
    }
}

impl Meet for StructuredType {
    fn meet_with(&mut self, other: &Self) {
        let mut rules = HashMap::new();
        let mut discovered_left = HashSet::new();
        let mut discovered_right = HashSet::new();
        let mut visit: Vec<NestedFunctor> = Vec::new();

        let mut visit_nodes = |left: &mut TypeNode, right: &TypeNode| {
            let mut res: Option<Vec<_>> = None;
            if matches!(left, TypeNode::Any) {
                discovered_right.extend(right.functors().iter().cloned());
            } else if matches!(right, TypeNode::Any) {
                discovered_left.extend(left.functors().iter().cloned());
            } else {
                res = Some(
                    left.functors()
                        .intersection(&right.functors())
                        .cloned()
                        .collect(),
                );
            }
            left.meet_with(&right);
            res.into_iter().flatten()
        };

        visit.extend(visit_nodes(&mut self.start, &other.start));

        while let Some(next) = visit.pop() {
            let hash_map::Entry::Vacant(entry) = rules.entry(next.clone()) else {
                continue;
            };

            let left = self.grammar.get(&next).unwrap();
            let right = other.grammar.get(&next).unwrap();
            let mut result = Vec::with_capacity(left.len());

            for (left_node, right_node) in left.iter().zip(right) {
                let mut res_node = left_node.clone();
                visit.extend(visit_nodes(&mut res_node, right_node));
                result.push(res_node);
            }

            entry.insert(result);
        }

        let mut left_grammar = TypeGrammar::new();
        left_grammar.union_with(self.grammar.sub_grammar(&discovered_left));
        left_grammar.union_with(other.grammar.sub_grammar(&discovered_right));
        for (f, node) in left_grammar.rules {
            if let hash_map::Entry::Vacant(entry) = rules.entry(f) {
                entry.insert(node);
            }
        }

        self.grammar = TypeGrammar { rules };
    }
}

impl Top for StructuredType {
    fn top() -> Self {
        Self {
            start: TypeNode::Any,
            grammar: TypeGrammar::new(),
        }
    }
}

impl InterpretBuiltin<NemoBuiltin> for StructuredType {
    fn interpret(builtin: NemoBuiltin, tup: Tup<Self>) -> Option<Tup<Self>> {
        todo!()
    }
}

impl Uncons<NemoFunctor> for StructuredType {
    fn uncons(&self, func: &NemoFunctor) -> Option<Vec<Self>> {
        let TypeNode::TypeNode(start) = &self.start else {
            return match func {
                NemoFunctor::Double => Some(vec![]),
                NemoFunctor::Const(_) => Some(vec![]),
                NemoFunctor::Nested(nested) => {
                    Some(repeat_with(Self::top).take(nested.arity()).collect())
                }
            };
        };

        let NemoFunctor::Nested(nested) = func else {
            let _ = start.flat_types.contains(func).then_some(())?;
            return Some(vec![]);
        };

        if !start.functors.contains(nested) {
            return None;
        }

        let subnodes = self
            .grammar
            .get(nested)
            .expect("func is principal functor, so it must have a description");

        let mut result = Vec::new();
        for subnode in subnodes {
            match subnode {
                TypeNode::Any => result.push(Self::top()),

                TypeNode::TypeNode(OrNode { functors, .. }) => {
                    let grammar = self.grammar.sub_grammar(functors);
                    let start = subnode.clone();
                    result.push(Self { start, grammar });
                }
            }
        }

        Some(result)
    }
}

impl Cons<NemoFunctor> for StructuredType {
    type Config = StructuredTypeConfig;

    fn cons(_config: &Self::Config, ctor: NemoFunctor, subterms: Vec<Self>) -> Option<Self> {
        let NemoFunctor::Nested(func) = ctor else {
            let flat_types = FlatType::from_constant(ctor);
            let start = TypeNode::TypeNode(OrNode {
                flat_types,
                functors: HashSet::new(),
            });

            return Some(Self {
                start,
                grammar: TypeGrammar::new(),
            });
        };

        let start = TypeNode::TypeNode(OrNode {
            flat_types: FlatType::bot(),
            functors: HashSet::from([func.clone()]),
        });

        let mut grammar = TypeGrammar::new();
        let mut start_rule = Vec::new();

        for subterm in subterms {
            grammar.union_with(subterm.grammar);
            start_rule.push(subterm.start);
        }

        grammar.add_rule(func, start_rule);
        Some(Self { start, grammar })
    }
}

impl Cons<NemoCtor> for StructuredType {
    type Config = StructuredTypeConfig;

    fn cons(config: &Self::Config, ctor: NemoCtor, subterms: Vec<Self>) -> Option<Self> {
        match ctor {
            NemoCtor::Aggregate => todo!(),
            NemoCtor::Null(_) => todo!(),
            NemoCtor::Functor(f) => Self::cons(config, f, subterms),
        }
    }
}

impl LocalMinimum<StructuredTypeConfig> for StructuredType {
    fn local_minimum(_key: &StructuredTypeConfig) -> Self {
        let start = TypeNode::TypeNode(OrNode {
            flat_types: FlatType::bot(),
            functors: HashSet::new(),
        });

        let grammar = TypeGrammar::new();
        Self { start, grammar }
    }
}

impl TypeDomain for StructuredType {
    type Model = NemoModel;

    type Config = StructuredTypeConfig;

    fn configure<P>(_program: &Program<P, NemoModel>) -> StructuredTypeConfig {
        StructuredTypeConfig {}
    }
}

#[cfg(test)]
mod test {
    use std::{
        collections::{HashMap, HashSet},
        vec,
    };

    use crate::{
        traits::{
            lattice::{Bottom, Meet, Union},
            structural::Uncons,
        },
        type_terms::{
            const_model::{IdentConstant, NemoFunctor, NestedFunctor},
            flat_type::FlatType,
            structured_type::OrNode,
        },
    };

    use super::{StructuredType, TypeGrammar, TypeNode};

    fn functor(name: &str, len: usize) -> NestedFunctor {
        NestedFunctor::List {
            tag: Some(name.into()),
            length: len,
        }
    }

    fn functor_node<T>(t: T) -> TypeNode
    where
        HashSet<NestedFunctor>: From<T>,
    {
        TypeNode::TypeNode(OrNode {
            flat_types: FlatType::bot(),
            functors: HashSet::from(t),
        })
    }

    fn flat_node(f: impl IntoIterator<Item = FlatType>) -> TypeNode {
        let flat_types = f.into_iter().fold(FlatType::bot(), |mut current, new| {
            current.union_with(&new);
            current
        });

        TypeNode::TypeNode(OrNode {
            flat_types,
            functors: HashSet::new(),
        })
    }

    #[test]
    fn uncons_list() {
        let list_cons = functor("::", 2);
        let nil = NemoFunctor::Const(IdentConstant::IriConst("<nil>".into()));

        let list_node = TypeNode::TypeNode(OrNode {
            flat_types: FlatType::from_constant(nil.clone()),
            functors: HashSet::from([list_cons.clone()]),
        });

        let list_grammar = TypeGrammar {
            rules: HashMap::from([(list_cons.clone(), vec![TypeNode::Any, list_node.clone()])]),
        };

        let structured_type = StructuredType {
            start: list_node.clone(),
            grammar: list_grammar,
        };

        let result = structured_type
            .uncons(&NemoFunctor::Nested(list_cons.clone()))
            .unwrap();
        assert_eq!(result.len(), 2);

        // check first value is top
        assert!(matches!(result[0].start, TypeNode::Any));
        assert_eq!(result[0].grammar.rules.len(), 0);

        // check second value is list
        let TypeNode::TypeNode(inner) = &result[1].start else {
            panic!("unexpected: list[1] = any")
        };

        assert_eq!(&inner.functors, &HashSet::from([list_cons]));
        assert_eq!(&inner.flat_types, &FlatType::from_constant(nil));
    }

    #[test]
    fn tricky_grammar_1() {
        // type_1: { f(any), g(foo) }
        // type_2: { f(g(any)) }
        // => type 1 > type 2

        let f = functor("f", 1);
        let g = functor("g", 1);

        let foo = functor_node([functor("foo", 0)]);

        let mut type_1 = StructuredType {
            start: functor_node([f.clone(), g.clone()]),
            grammar: TypeGrammar {
                rules: HashMap::from([(f.clone(), vec![TypeNode::Any]), (g.clone(), vec![foo])]),
            },
        };

        let type_2 = StructuredType {
            start: TypeNode::TypeNode(OrNode {
                flat_types: FlatType::bot(),
                functors: HashSet::from([f.clone()]),
            }),
            grammar: TypeGrammar {
                rules: HashMap::from([
                    (f.clone(), vec![functor_node([g.clone()])]),
                    (g.clone(), vec![TypeNode::Any]),
                ]),
            },
        };

        assert!(type_1 > type_2);
        assert!(type_2 < type_1);
        type_1.meet_with(&type_2);
        assert_eq!(type_1, type_2);
    }

    #[test]
    fn tricky_grammar_2() {
        // type_1: { f(any) }
        // type_2: { f(g(any)) }
        // => type 1 > type 2

        let f = functor("f", 1);
        let g = functor("g", 1);

        let mut type_1 = StructuredType {
            start: functor_node([f.clone()]),
            grammar: TypeGrammar {
                rules: HashMap::from([(f.clone(), vec![TypeNode::Any])]),
            },
        };

        let type_2 = StructuredType {
            start: functor_node([f.clone()]),
            grammar: TypeGrammar {
                rules: HashMap::from([
                    (f.clone(), vec![functor_node([g.clone()])]),
                    (g.clone(), vec![TypeNode::Any]),
                ]),
            },
        };

        assert!(type_1 > type_2);
        assert!(type_2 < type_1);
        type_1.meet_with(&type_2);
        assert_eq!(type_1, type_2);
    }

    #[test]
    fn meet_2() {
        // {a(f(int)) + b(*)} `meet` {a(*) + b(f(str))} -> a(f(int, str)) | b(f(int, str))

        // type_1 = {a(f(int)) + b(*)}
        let mut type_1 = StructuredType {
            start: functor_node([functor("a", 1), functor("b", 1)]),
            grammar: TypeGrammar {
                rules: HashMap::from([
                    (functor("a", 1), vec![functor_node([functor("f", 1)])]),
                    (functor("f", 1), vec![flat_node([FlatType::int()])]),
                    (functor("b", 1), vec![TypeNode::Any]),
                ]),
            },
        };

        // type_2 = {a(*) + b(f(str))}
        let type_2 = StructuredType {
            start: functor_node([functor("a", 1), functor("b", 1)]),
            grammar: TypeGrammar {
                rules: HashMap::from([
                    (functor("b", 1), vec![functor_node([functor("f", 1)])]),
                    (functor("f", 1), vec![flat_node([FlatType::str()])]),
                    (functor("a", 1), vec![TypeNode::Any]),
                ]),
            },
        };

        // expected = a(f(int, str)) | b(f(int, str))
        let expected = StructuredType {
            start: functor_node([functor("a", 1), functor("b", 1)]),
            grammar: TypeGrammar {
                rules: HashMap::from([
                    (functor("a", 1), vec![functor_node([functor("f", 1)])]),
                    (functor("b", 1), vec![functor_node([functor("f", 1)])]),
                    (
                        functor("f", 1),
                        vec![flat_node([FlatType::str(), FlatType::int()])],
                    ),
                ]),
            },
        };

        type_1.meet_with(&type_2);
        assert_eq!(type_1, expected);
    }
}
