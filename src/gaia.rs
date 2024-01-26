use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

use crate::fixpoint::MonotoneTransform;
use crate::lattice::{LubIterator, PreOrder};

use self::abstract_substitution::{AbstractSubstitution, Query};

mod abstract_substitution;
mod bit_substitution;
mod type_table;

#[derive(Debug, Clone)]
pub enum NemoFunctor {
    Double,
    StrConst(StrConst),
    IntConst(IntConst),
    RdfConst(RdfConst),
    Null(NullGenerator),
    Map {
        tag: Option<TermTag>,
        keys: Arc<[MapKey]>,
    },
    List {
        tag: Option<TermTag>,
        length: usize,
    },
}

type IntConst = i64;
type StrConst = Arc<str>;
type RdfConst = Arc<str>;
type NullGenerator = usize;
type MapKey = Arc<str>;
type TermTag = Arc<str>;
type Variable = u16;

struct NormalizedClause<F, P> {
    body_atoms: Vec<(P, Vec<Variable>)>,
    variable_equalities: Vec<(Variable, Variable)>,
    ctor_equalities: Vec<(Variable, F, Vec<Variable>)>,
    num_variables: u16,
}

impl<F, P> NormalizedClause<F, P> {
    fn execute<S>(&self, mut f: impl FnMut(Query<P, S>) -> S, input: &S) -> S
    where
        P: Clone,
        S: AbstractSubstitution<F, P>,
    {
        let mut subst = input.extended(self.num_variables);

        for &(lhs, rhs) in &self.variable_equalities {
            subst.apply_eq(lhs, rhs);
        }

        for (lhs, f, vars) in &self.ctor_equalities {
            subst.apply_func(*lhs, f, vars);
        }

        for (pred, vars) in &self.body_atoms {
            let local_subst = subst.project(vars.iter().cloned());
            let query = Query {
                predicate: pred.clone(),
                subst: local_subst,
            };
            subst.propagate(vars.iter().cloned(), f(query));
        }

        subst.restrict(input.len());
        subst
    }
}

pub struct Gaia<F, P> {
    program: HashMap<(P, Variable), Vec<NormalizedClause<F, P>>>,
}

pub trait Clause: Sized {
    type PredicateSymbol: Hash + Eq + Debug + Clone;
    type FunctionSymbol;
}

impl<F, P, S> MonotoneTransform<Query<P, S>> for Gaia<F, P>
where
    S: AbstractSubstitution<F, P>,
    P: Eq + Hash + Clone,
{
    type Output = S;

    fn call<Func>(&self, mut f: Func, query: Query<P, S>) -> Self::Output
    where
        Func: FnMut(Query<P, S>) -> Self::Output,
    {
        let min = S::local_minimum(&query);
        self.program
            .get(&(query.predicate, query.subst.len()))
            .into_iter()
            .flatten()
            .map(|c| c.execute(&mut f, &query.subst))
            .lub(min)
    }
}

pub mod test {
    use std::{collections::HashMap, sync::Arc};

    use crate::{
        fixpoint::{compute_fixpoint, MonotoneTransform},
        gaia::{
            abstract_substitution::{AbstractSubstitution, Query},
            bit_substitution::{BitSubstitution, TypeDescriptor},
            Gaia, NormalizedClause,
        },
    };

    pub fn test_main() {
        reach_with_backtrack()
    }

    fn reach_with_backtrack() {
        use super::NemoFunctor;

        fn nm_str(s: &str) -> NemoFunctor {
            NemoFunctor::StrConst(s.into())
        }

        fn nm_int(i: i64) -> NemoFunctor {
            NemoFunctor::IntConst(i)
        }

        let mut program: HashMap<_, Vec<NormalizedClause<NemoFunctor, _>>> = HashMap::new();

        let mut push_fact = |pred, vals: Vec<_>| {
            program.entry(pred).or_default().push(NormalizedClause {
                body_atoms: vec![],
                variable_equalities: vec![],
                ctor_equalities: vals
                    .into_iter()
                    .enumerate()
                    .map(|(i, v)| (i as u16, v, vec![]))
                    .collect(),
                num_variables: 3,
            });
        };

        #[cfg(unused)]
        {
            let edge_3 = ("edge", 3);
            push_fact(edge_3, vec![nm_str("a"), nm_int(2), nm_str("b")]);
            push_fact(edge_3, vec![nm_str("b"), nm_int(3), nm_str("c")]);
            push_fact(edge_3, vec![nm_str("b"), nm_int(5), nm_str("d")]);
            push_fact(edge_3, vec![nm_str("c"), nm_int(1), nm_str("d")]);
            push_fact(edge_3, vec![nm_str("c"), nm_int(7), nm_str("e")]);
            push_fact(edge_3, vec![nm_str("d"), nm_int(5), nm_str("f")]);
            push_fact(edge_3, vec![nm_str("e"), nm_int(2), nm_str("f")]);
            push_fact(edge_3, vec![nm_str("a"), nm_int(11), nm_str("f")]);

            let start = ("start", 1);
            let end = ("end", 1);

            push_fact(start, vec![nm_str("a")]);
            push_fact(end, vec![nm_str("f")]);
        }

        push_fact(("p", 2), vec![nm_str("a"), nm_str("b")]);
        push_fact(("p", 2), vec![nm_int(2), nm_int(3)]);

        program.entry(("q", 2)).or_default().push(NormalizedClause {
            body_atoms: vec![("p", vec![0, 1])],
            variable_equalities: vec![],
            ctor_equalities: vec![(0, nm_int(2), vec![])],
            num_variables: 2,
        });

        let g = Gaia { program };

        let mut td = TypeDescriptor::default();
        // td.str_constants.push("a".into());
        let query = Query {
            subst: BitSubstitution::any(2, Arc::new(td)),
            predicate: "q",
        };

        let (pt, _) = compute_fixpoint(query, g);
        println!("{pt:#?}");
    }
}
