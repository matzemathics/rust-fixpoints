use fixpoints::{
    type_inference::{
        fixpoint::compute_fixpoint,
        model::{BodyAtom, BodyBuiltin, BodyTerm, HeadTerm, PatClause},
        Program,
    },
    type_terms::{
        const_model::{IdentConstant, NemoBuiltin, NemoCtor, TermLike},
        flat_type::{FlatType, WildcardType},
        structured_type::StructuredType,
    },
};

fn main() {
    let mut program = Program::new();

    fn num<T: TermLike>(i: i64) -> T {
        T::constant(IdentConstant::IntConst(i))
    }
    fn strng<T: TermLike>(s: &str) -> T {
        T::constant(IdentConstant::StrConst(s.into()))
    }
    fn var<T: TermLike>(v: u16) -> T {
        T::variable(v)
    }
    fn fun<T: TermLike>(s: &str, v: Vec<T>) -> T {
        T::functor(s.into(), v)
    }

    let fact = |head| PatClause {
        head,
        body_atoms: vec![],
        body_builtins: vec![],
        body_variables: 0,
    };

    // import p(int, str).
    program.add_rule(
        "p",
        PatClause {
            head: vec![var(0), var(1)],
            body_atoms: vec![],
            body_builtins: vec![BodyBuiltin {
                builtin: NemoBuiltin::Import(vec![FlatType::int(), FlatType::str()]),
                terms: vec![var(0), var(1)],
            }],
            body_variables: 2,
        },
    );

    // a("test")
    // a(3)
    program.add_rule("a", fact(vec![strng("test")]));
    program.add_rule("a", fact(vec![num(3)]));


    // r(?x, a(?y)) :- p(?x, ?y), a(?x).
    program.add_rule(
        "r",
        PatClause {
            head: vec![var(0), fun("f", vec![var(1)])],
            body_atoms: vec![BodyAtom {
                predicate: "p",
                terms: vec![var(0), var(1)],
            }, BodyAtom {
                predicate: "a",
                terms: vec![var(0)]
            }],
            body_builtins: vec![],
            body_variables: 2,
        },
    );

    // o(?x) :- r(?x, a(_)).
    program.add_rule(
        "o",
        PatClause {
            head: vec![var(0)],
            body_atoms: vec![BodyAtom {
                predicate: "r",
                terms: vec![var(0), fun("f", vec![BodyTerm::DontCare])],
            }],
            body_builtins: vec![],
            body_variables: 1,
        },
    );

    let analysis = program.analyse::<StructuredType<FlatType>>();
    let fixpoint = compute_fixpoint("o", analysis.clone());
    println!("{:#?}", fixpoint.deps);
    println!("{:#?}", fixpoint.map);
    let result = analysis.backwards(fixpoint.deps, "o", fixpoint.map);
    println!("{result:#?}");
}
