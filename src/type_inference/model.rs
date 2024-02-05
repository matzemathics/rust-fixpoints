use crate::traits::structural::RuleModel;

pub type Variable = u16;

pub(super) enum HeadTerm<C> {
    Var(Variable),
    NemoCtor(C, Vec<HeadTerm<C>>),
}

pub(super) enum BodyTerm<F> {
    Var(Variable),
    Functor {
        functor: F,
        subterms: Vec<BodyTerm<F>>,
    },
    DontCare,
}

pub(super) struct BodyAtom<F, P> {
    pub(super) predicate: P,
    pub(super) terms: Vec<BodyTerm<F>>,
}

pub(super) struct BodyBuiltin<F, Builtin> {
    pub(super) builtin: Builtin,
    pub(super) terms: Vec<BodyTerm<F>>,
}

pub(super) struct PatClause<M: RuleModel> {
    pub(super) head: Vec<HeadTerm<M::Constructor>>,
    pub(super) body_atoms: Vec<BodyAtom<M::Functor, M::Predicate>>,
    pub(super) body_builtins: Vec<BodyBuiltin<M::Functor, M::Builtin>>,
    pub(super) body_variables: u16,
}
