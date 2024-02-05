use crate::NemoFunctor;

fn main() {
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
