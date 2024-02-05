use crate::traits::structural::ConstModel;

pub type Variable = u16;

pub enum HeadTerm<C> {
    Var(Variable),
    NemoCtor(C, Vec<HeadTerm<C>>),
}

pub enum BodyTerm<F> {
    Var(Variable),
    Functor {
        functor: F,
        subterms: Vec<BodyTerm<F>>,
    },
    DontCare,
}

pub struct BodyAtom<F, P> {
    pub predicate: P,
    pub terms: Vec<BodyTerm<F>>,
}

pub struct BodyBuiltin<F, Builtin> {
    pub builtin: Builtin,
    pub terms: Vec<BodyTerm<F>>,
}

pub struct PatClause<Predicate, C: ConstModel> {
    pub head: Vec<HeadTerm<C::Constructor>>,
    pub body_atoms: Vec<BodyAtom<C::Functor, Predicate>>,
    pub body_builtins: Vec<BodyBuiltin<C::Functor, C::Builtin>>,
    pub body_variables: u16,
}
