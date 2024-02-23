pub type Variable = u16;

#[derive(Debug, Clone)]
pub enum HeadTerm<C> {
    Var(Variable),
    Ctor(C, Vec<HeadTerm<C>>),
}

#[derive(Debug, Clone)]
pub enum BodyTerm<F> {
    Var(Variable),
    Functor {
        functor: F,
        subterms: Vec<BodyTerm<F>>,
    },
    DontCare,
}

#[derive(Debug, Clone)]
pub struct BodyAtom<F, P> {
    pub predicate: P,
    pub terms: Vec<BodyTerm<F>>,
}

#[derive(Debug, Clone)]
pub struct BodyBuiltin<F, Builtin> {
    pub builtin: Builtin,
    pub terms: Vec<BodyTerm<F>>,
}

#[derive(Debug, Clone)]
pub struct PatClause<Predicate, Functor, Constructor, Builtin> {
    pub head: Vec<HeadTerm<Constructor>>,
    pub body_atoms: Vec<BodyAtom<Functor, Predicate>>,
    pub body_builtins: Vec<BodyBuiltin<Functor, Builtin>>,
    pub body_variables: u16,
}

#[cfg(unused)]
impl<P, C: ConstModel> PatClause<P, C> {
    pub(crate) fn head_ctors(&self) -> impl Iterator<Item = C::Constructor> {
        struct Dfs<Ctor> {
            stack: Vec<HeadTerm<Ctor>>,
        }

        impl<Ctor> Iterator for Dfs<Ctor> {
            type Item = Ctor;

            fn next(&mut self) -> Option<Self::Item> {
                loop {
                    let node = self.stack.pop()?;

                    match node {
                        HeadTerm::Var(_) => continue,
                        HeadTerm::Ctor(ctor, subterms) => {
                            self.stack.extend(subterms);
                            break Some(ctor);
                        }
                    }
                }
            }
        }

        Dfs {
            stack: self.head.clone(),
        }
    }

    pub(crate) fn body_functors(&self) -> impl Iterator<Item = C::Functor> {
        struct Dfs<T> {
            stack: Vec<BodyTerm<T>>,
        }

        impl<F> Iterator for Dfs<F> {
            type Item = F;

            fn next(&mut self) -> Option<Self::Item> {
                loop {
                    let node = self.stack.pop()?;

                    match node {
                        BodyTerm::Var(_) => continue,
                        BodyTerm::Functor { functor, subterms } => {
                            self.stack.extend(subterms);
                            break Some(functor);
                        }
                        BodyTerm::DontCare => continue,
                    }
                }
            }
        }

        let atoms = self
            .body_atoms
            .iter()
            .flat_map(|BodyAtom { terms, .. }| terms)
            .cloned();

        let builtins = self
            .body_builtins
            .iter()
            .flat_map(|BodyBuiltin { terms, .. }| terms)
            .cloned();

        Dfs {
            stack: atoms.chain(builtins).collect(),
        }
    }
}
