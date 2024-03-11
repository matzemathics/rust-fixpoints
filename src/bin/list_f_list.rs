use fixpoints::{
    type_inference::{
        fixpoint::compute_fixpoint,
        model::{BodyAtom, BodyTerm, HeadTerm, PatClause},
        Program,
    },
    type_terms::{
        const_model::{IdentConstant, NemoCtor, NemoFunctor, NestedFunctor},
        flat_type::FlatType,
        structured_type::StructuredType,
    },
};

fn main() {
    let mut program = Program::new();
    let nil = NemoFunctor::Const(IdentConstant::IriConst("<nil>".into()));
    let cons = NemoFunctor::Nested(NestedFunctor::List {
        tag: Some("::".into()),
        length: 2,
    });
    let double_cons = NemoFunctor::Nested(NestedFunctor::List {
        tag: Some(":::".into()),
        length: 2,
    });

    let nil_clause = || PatClause {
        head: vec![HeadTerm::Ctor(NemoCtor::Functor(nil.clone()), vec![])],
        body_atoms: vec![],
        body_builtins: vec![],
        body_variables: 0,
    };

    // list(nil).
    program.add_rule("list", nil_clause());

    // list(__double__, ?xs) :- list(?xs).
    program.add_rule(
        "list",
        PatClause {
            head: vec![HeadTerm::Ctor(
                NemoCtor::Functor(cons.clone()),
                vec![
                    HeadTerm::Ctor(NemoCtor::Functor(NemoFunctor::Double), vec![]),
                    HeadTerm::Var(0),
                ],
            )],
            body_atoms: vec![BodyAtom {
                predicate: "list",
                terms: vec![BodyTerm::Var(0)],
            }],
            body_builtins: vec![],
            body_variables: 1,
        },
    );

    // list_list(nil).
    program.add_rule("list_list", nil_clause());

    // list_list(?x, ?xs) :- list(?x), list_list(?xs).
    program.add_rule(
        "list_list",
        PatClause {
            head: vec![HeadTerm::Ctor(
                NemoCtor::Functor(double_cons),
                vec![HeadTerm::Var(0), HeadTerm::Var(1)],
            )],
            body_atoms: vec![
                BodyAtom {
                    predicate: "list",
                    terms: vec![BodyTerm::Var(0)],
                },
                BodyAtom {
                    predicate: "list_list",
                    terms: vec![BodyTerm::Var(1)],
                },
            ],
            body_builtins: vec![],
            body_variables: 2,
        },
    );

    let pt = compute_fixpoint("list_list", program.analyse::<StructuredType<FlatType>>());
    println!("{pt:#?}");
}
