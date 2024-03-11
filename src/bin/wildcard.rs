use fixpoints::{
    type_inference::{
        fixpoint::compute_fixpoint,
        model::{BodyAtom, BodyTerm, PatClause},
        Program,
    },
    type_terms::{
        const_model::{IdentConstant, NemoFunctor, NestedFunctor, TermLike},
        flat_type::FlatType,
        structured_type::StructuredType,
    },
};

fn main() {
    let mut program = Program::new();
    let _nil = NemoFunctor::Const(IdentConstant::IriConst("<nil>".into()));
    let _cons = NemoFunctor::Nested(NestedFunctor::List {
        tag: Some("::".into()),
        length: 2,
    });

    fn num<T: TermLike>(i: i64) -> T {
        T::constant(IdentConstant::IntConst(i))
    }
    fn strng<T: TermLike>(s: &str) -> T {
        T::constant(IdentConstant::StrConst(s.into()))
    }
    fn var<T: TermLike>(v: u16) -> T {
        T::variable(v)
    }

    let fact = |head| PatClause {
        head,
        body_atoms: vec![],
        body_builtins: vec![],
        body_variables: 0,
    };

    // a(1, "test")
    // a(2, 3)
    program.add_rule("a", fact(vec![num(1), strng("test")]));
    program.add_rule("a", fact(vec![num(2), num(3)]));

    // r(?x) :- a(2, ?x).
    program.add_rule(
        "r",
        PatClause {
            head: vec![var(0)],
            body_atoms: vec![BodyAtom {
                predicate: "a",
                terms: vec![num(2), var(0)],
            }],
            body_builtins: vec![],
            body_variables: 1,
        },
    );

    // r(42) :- a(1, _).
    program.add_rule(
        "r",
        PatClause {
            head: vec![num(42)],
            body_atoms: vec![BodyAtom {
                predicate: "a",
                terms: vec![num(1), BodyTerm::DontCare],
            }],
            body_builtins: vec![],
            body_variables: 0,
        },
    );

    let analysis = program.analyse::<StructuredType<FlatType>>();
    let fixpoint = compute_fixpoint("r", analysis.clone());
    let result = analysis.backwards(fixpoint.deps, "r", fixpoint.map);
    println!("{result:#?}");
}
