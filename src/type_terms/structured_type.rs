use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    hash::Hash,
    iter::repeat_with,
    vec,
};

use crate::{
    traits::{
        lattice::{LocalMinimum, Meet, PreOrder, Top},
        structural::{Cons, InterpretBuiltin, TypeDomain, Uncons},
    },
    type_inference::Program,
    util::tup::Tup,
};

use super::{
    const_model::{NemoBuiltin, NemoCtor, NemoFunctor, NemoModel, NestedFunctor},
    flat_type::{FlatType, FlatTypeConfig},
};

#[derive(Debug, Clone)]
pub struct StructuredType {
    pub(self) start: TypeNode,
    pub(self) grammar: TypeGrammar,
}

#[derive(Debug, Clone)]
enum TypeNode {
    Any,
    TypeNode {
        flat_types: FlatType,
        principal_functors: HashSet<NestedFunctor>,
    },
}

#[derive(Debug, Clone)]
struct TypeGrammar(HashMap<NestedFunctor, Vec<TypeNode>>);

impl TypeGrammar {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn get(&self, functor: &NestedFunctor) -> Option<&[TypeNode]> {
        self.0.get(functor).map(Vec::as_slice)
    }

    fn sub_grammar<'a>(&self, funcs: impl IntoIterator<Item = &'a NestedFunctor>) -> Self {
        let mut visit = Vec::from_iter(funcs);
        let mut result = HashMap::new();

        while let Some(current) = visit.pop() {
            let Entry::Vacant(entry) = result.entry(current.clone()) else {
                continue;
            };

            let rhs = self
                .0
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

        Self(result)
    }
}

#[derive(Debug, Clone)]
pub struct StructuredTypeConfig {
    flat_config: FlatTypeConfig,
}

impl PreOrder for StructuredType {
    fn leq(&self, other: &Self) -> bool {
        todo!()
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
            let _ = flat_types.uncons(func)?;
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
            let flat_types = FlatType::cons(&config.flat_config, ctor, vec![])?;
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
            flat_types: FlatType::local_minimum(&config.flat_config),
            principal_functors: HashSet::from([func]),
        };

        todo!()
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
    fn local_minimum(key: &StructuredTypeConfig) -> Self {
        todo!()
    }
}

impl TypeDomain for StructuredType {
    type Model = NemoModel;

    type Config = StructuredTypeConfig;

    fn configure<P>(program: &Program<P, NemoModel>) -> StructuredTypeConfig {
        todo!()
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
            lattice::Top,
            structural::{Cons, Uncons},
        },
        type_terms::{
            const_model::{IdentConstant, NemoFunctor, NestedFunctor},
            flat_type::{FlatType, FlatTypeConfig},
        },
    };

    use super::{StructuredType, TypeGrammar, TypeNode};

    #[test]
    fn uncons_list() {
        let list_cons = NestedFunctor::List {
            tag: Some("::".into()),
            length: 2,
        };

        let nil = NemoFunctor::Const(IdentConstant::RdfConst("<nil>".into()));
        let flat_type_config = FlatTypeConfig::const_set();

        let list_node = TypeNode::TypeNode {
            flat_types: FlatType::cons(&flat_type_config, nil, vec![]).unwrap(),
            principal_functors: HashSet::from([list_cons.clone()]),
        };

        let list_grammar = TypeGrammar(HashMap::from([(
            list_cons.clone(),
            vec![TypeNode::Any, list_node.clone()],
        )]));

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
        assert_eq!(result[0].grammar.0.len(), 0);

        // check second value is list
        let TypeNode::TypeNode {
            flat_types,
            principal_functors,
        } = &result[1].start
        else {
            panic!("unexpected: list[1] = any")
        };

        assert_eq!(principal_functors, &HashSet::from([list_cons]));
        // todo: check flat_types = { nil }
    }
}
