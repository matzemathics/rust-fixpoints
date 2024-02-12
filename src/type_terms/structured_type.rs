use std::{
    cmp::Ordering,
    collections::{hash_map, HashMap, HashSet},
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum TypeNode {
    Any,
    TypeNode {
        flat_types: FlatType,
        principal_functors: HashSet<NestedFunctor>,
    },
}

impl PartialOrd for TypeNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (TypeNode::Any, TypeNode::Any) => Some(Ordering::Equal),
            (TypeNode::Any, TypeNode::TypeNode { .. }) => Some(Ordering::Greater),
            (TypeNode::TypeNode { .. }, TypeNode::Any) => Some(Ordering::Less),
            (
                TypeNode::TypeNode {
                    flat_types,
                    principal_functors,
                },
                TypeNode::TypeNode {
                    flat_types: other_flat_types,
                    principal_functors: other_principal_functors,
                },
            ) => {
                let func_ge = principal_functors
                    .difference(other_principal_functors)
                    .next()
                    .is_none();
                let func_le = other_principal_functors
                    .difference(principal_functors)
                    .next()
                    .is_none();

                match (func_ge, func_le) {
                    (true, true) => flat_types.partial_cmp(other_flat_types),
                    (true, false) => flat_types.ge(other_flat_types).then_some(Ordering::Greater),
                    (false, true) => flat_types.le(other_flat_types).then_some(Ordering::Less),
                    (false, false) => None,
                }
            }
        }
    }
}

impl Meet for TypeNode {
    fn meet_with(&mut self, other: &Self) {
        let TypeNode::TypeNode {
            flat_types,
            principal_functors,
        } = self
        else {
            *self = other.clone();
            return;
        };

        let TypeNode::TypeNode {
            flat_types: other_flat_types,
            principal_functors: other_functors,
        } = other
        else {
            return;
        };

        flat_types.meet_with(other_flat_types);
        principal_functors.retain(|f| other_functors.contains(f))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TypeGrammar {
    rules: HashMap<NestedFunctor, Vec<TypeNode>>,
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
                let TypeNode::TypeNode {
                    principal_functors, ..
                } = node
                else {
                    continue;
                };

                visit.extend(principal_functors)
            }
        }

        Self { rules }
    }

    fn add_rule(
        &mut self,
        _config: &StructuredTypeConfig,
        func: NestedFunctor,
        args: Vec<TypeNode>,
    ) {
        let mut entry = match self.rules.entry(func) {
            hash_map::Entry::Vacant(entry) => {
                entry.insert(args);
                return;
            }
            hash_map::Entry::Occupied(entry) => entry,
        };

        for (current, new) in entry.get_mut().iter_mut().zip(args) {
            let TypeNode::TypeNode {
                flat_types,
                principal_functors,
            } = current
            else {
                continue;
            };

            let TypeNode::TypeNode {
                flat_types: new_flat_types,
                principal_functors: new_functors,
            } = new
            else {
                *current = TypeNode::Any;
                continue;
            };

            flat_types.union_with(&new_flat_types);
            principal_functors.extend(new_functors);
        }
    }

    fn union_with(&mut self, config: &StructuredTypeConfig, other: TypeGrammar) {
        for (func, args) in other.rules {
            self.add_rule(config, func, args)
        }
    }
}

impl Meet for TypeGrammar {
    fn meet_with(&mut self, other: &Self) {
        let keys: HashSet<_> = self.rules.keys().chain(other.rules.keys()).collect();
        todo!()
    }
}

impl PartialOrd for TypeGrammar {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let mut comparison = ThreeWayCompare::init();
        let keys: HashSet<_> = self.rules.keys().chain(other.rules.keys()).collect();

        for key in keys {
            let Some(left) = self.get(key) else {
                continue;
            };

            let Some(right) = other.get(key) else {
                continue;
            };

            for (l, r) in left.iter().zip(right) {
                comparison = comparison.chain(l, r)?
            }
        }

        Some(comparison.finish())
    }
}

#[derive(Debug, Clone)]
pub struct StructuredTypeConfig {}

impl PartialOrd for StructuredType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        ThreeWayCompare::init()
            .chain(&self.start, &other.start)?
            .chain(&self.grammar, &other.grammar)
            .map(ThreeWayCompare::finish)
    }
}

impl Meet for StructuredType {
    fn meet_with(&mut self, other: &Self) {
        todo!()
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
        let TypeNode::TypeNode {
            principal_functors,
            flat_types,
        } = &self.start
        else {
            return match func {
                NemoFunctor::Double => Some(vec![]),
                NemoFunctor::Const(_) => Some(vec![]),
                NemoFunctor::Nested(nested) => {
                    Some(repeat_with(Self::top).take(nested.arity()).collect())
                }
            };
        };

        let NemoFunctor::Nested(nested) = func else {
            let _ = flat_types.contains(func).then_some(())?;
            return Some(vec![]);
        };

        if !principal_functors.contains(nested) {
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

                TypeNode::TypeNode {
                    principal_functors, ..
                } => {
                    let grammar = self.grammar.sub_grammar(principal_functors);
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

    fn cons(config: &Self::Config, ctor: NemoFunctor, subterms: Vec<Self>) -> Option<Self> {
        let NemoFunctor::Nested(func) = ctor else {
            let flat_types = FlatType::from_constant(ctor);
            let start = TypeNode::TypeNode {
                flat_types,
                principal_functors: HashSet::new(),
            };

            return Some(Self {
                start,
                grammar: TypeGrammar::new(),
            });
        };

        let start = TypeNode::TypeNode {
            flat_types: FlatType::bot(),
            principal_functors: HashSet::from([func.clone()]),
        };

        let mut grammar = TypeGrammar::new();
        let mut start_rule = Vec::new();

        for subterm in subterms {
            grammar.union_with(config, subterm.grammar);
            start_rule.push(subterm.start);
        }

        grammar.add_rule(config, func, start_rule);
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
        let start = TypeNode::TypeNode {
            flat_types: FlatType::bot(),
            principal_functors: HashSet::new(),
        };
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
        traits::{lattice::Bottom, structural::Uncons},
        type_terms::{
            const_model::{IdentConstant, NemoFunctor, NestedFunctor},
            flat_type::FlatType,
        },
    };

    use super::{StructuredType, TypeGrammar, TypeNode};

    #[test]
    fn uncons_list() {
        let list_cons = NestedFunctor::List {
            tag: Some("::".into()),
            length: 2,
        };

        let nil = NemoFunctor::Const(IdentConstant::IriConst("<nil>".into()));

        let list_node = TypeNode::TypeNode {
            flat_types: FlatType::from_constant(nil.clone()),
            principal_functors: HashSet::from([list_cons.clone()]),
        };

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
        let TypeNode::TypeNode {
            flat_types,
            principal_functors,
        } = &result[1].start
        else {
            panic!("unexpected: list[1] = any")
        };

        assert_eq!(principal_functors, &HashSet::from([list_cons]));
        assert_eq!(flat_types, &FlatType::from_constant(nil));
    }

    #[test]
    fn tricky_grammar_1() {
        // type_1: { f(any), g(foo) }
        // type_2: { f(g(any)) }
        // clearly type 1 > type 2

        let f = NestedFunctor::List {
            tag: Some("f".into()),
            length: 1,
        };

        let g = NestedFunctor::List {
            tag: Some("g".into()),
            length: 1,
        };

        let foo = TypeNode::TypeNode {
            flat_types: FlatType::bot(),
            principal_functors: HashSet::from([NestedFunctor::List {
                tag: Some("foo".into()),
                length: 0,
            }]),
        };

        let type_1 = StructuredType {
            start: TypeNode::TypeNode {
                flat_types: FlatType::bot(),
                principal_functors: HashSet::from([f.clone(), g.clone()]),
            },
            grammar: TypeGrammar {
                rules: HashMap::from([(f.clone(), vec![TypeNode::Any]), (g.clone(), vec![foo])]),
            },
        };

        let type_2 = StructuredType {
            start: TypeNode::TypeNode {
                flat_types: FlatType::bot(),
                principal_functors: HashSet::from([f.clone()]),
            },
            grammar: TypeGrammar {
                rules: HashMap::from([
                    (
                        f.clone(),
                        vec![TypeNode::TypeNode {
                            flat_types: FlatType::bot(),
                            principal_functors: HashSet::from([g.clone()]),
                        }],
                    ),
                    (g.clone(), vec![TypeNode::Any]),
                ]),
            },
        };

        assert!(type_1 > type_2);
    }

    #[test]
    fn tricky_grammar_2() {
        // type_1: { f(any) }
        // type_2: { f(g(any)) }
        // clearly type 1 > type 2

        let f = NestedFunctor::List {
            tag: Some("f".into()),
            length: 1,
        };

        let g = NestedFunctor::List {
            tag: Some("g".into()),
            length: 1,
        };

        let type_1 = StructuredType {
            start: TypeNode::TypeNode {
                flat_types: FlatType::bot(),
                principal_functors: HashSet::from([f.clone()]),
            },
            grammar: TypeGrammar {
                rules: HashMap::from([(f.clone(), vec![TypeNode::Any])]),
            },
        };

        let type_2 = StructuredType {
            start: TypeNode::TypeNode {
                flat_types: FlatType::bot(),
                principal_functors: HashSet::from([f.clone()]),
            },
            grammar: TypeGrammar {
                rules: HashMap::from([
                    (
                        f.clone(),
                        vec![TypeNode::TypeNode {
                            flat_types: FlatType::bot(),
                            principal_functors: HashSet::from([g.clone()]),
                        }],
                    ),
                    (g.clone(), vec![TypeNode::Any]),
                ]),
            },
        };

        assert!(type_1 > type_2);
    }
}
