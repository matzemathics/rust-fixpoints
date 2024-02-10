use std::vec;

use fixpoints::type_inference::fixpoint::compute_fixpoint;
use fixpoints::type_inference::model::{BodyAtom, BodyTerm, HeadTerm, PatClause};
use fixpoints::type_inference::Program;
use fixpoints::type_terms::const_model::{IdentConstant, NemoCtor, NemoFunctor};
use fixpoints::type_terms::old_flat_type::FlatType;

fn main() {
    fn nm_str(s: &str) -> NemoFunctor {
        NemoFunctor::Const(IdentConstant::StrConst(s.into()))
    }

    fn nm_int(i: i64) -> NemoFunctor {
        NemoFunctor::Const(IdentConstant::IntConst(i))
    }

    let mut program: Program<_, _> = Program::new();

    let fact_clause = |vals: Vec<_>| PatClause {
        head: vals
            .into_iter()
            .map(|v| HeadTerm::Ctor(NemoCtor::Functor(v), vec![]))
            .collect(),
        body_builtins: vec![],
        body_atoms: vec![],
        body_variables: 0,
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

    program.add_rule(("p", 2), fact_clause(vec![nm_str("a"), nm_str("b")]));
    program.add_rule(("p", 2), fact_clause(vec![nm_int(2), nm_int(3)]));

    program.add_rule(
        ("q", 2),
        PatClause {
            head: vec![HeadTerm::Var(0), HeadTerm::Var(1)],
            body_atoms: vec![BodyAtom {
                predicate: ("p", 2),
                terms: vec![BodyTerm::Var(0), BodyTerm::Var(1)],
            }],
            body_builtins: vec![],
            body_variables: 2,
        },
    );

    let pt = compute_fixpoint(("q", 2), program.analyse::<FlatType>());
    println!("{pt:#?}");
}
