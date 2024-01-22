use crate::lattice::PreOrder;

enum Type<F> {
    Any,
    Func(F, Vec<Type<F>>),
}

struct TypeGraph {}

struct Functor {
    tag: Option<String>,
    args: FunctorArguments,
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum FunctorArguments {
    List { arity: u16 },
    Map { keys: Vec<(String, Necessity)> },
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum Necessity {
    Optional,
    Required,
}

impl PartialOrd for Necessity {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match self {
            Necessity::Optional => match other {
                Necessity::Optional => Some(std::cmp::Ordering::Equal),
                Necessity::Required => Some(std::cmp::Ordering::Less),
            },
            Necessity::Required => match other {
                Necessity::Optional => Some(std::cmp::Ordering::Greater),
                Necessity::Required => Some(std::cmp::Ordering::Equal),
            },
        }
    }
}

impl PreOrder for Functor {
    fn leq(&self, other: &Self) -> bool {
        if self.tag != other.tag {
            return false;
        }

        match &self.args {
            FunctorArguments::List { arity } => {
                other.args.eq(&FunctorArguments::List { arity: *arity })
            }
            FunctorArguments::Map { keys } => {
                let mut left = keys.iter();
                let FunctorArguments::Map { keys } = &other.args else {
                    return false;
                };
                let mut right = keys.iter();

                while let Some((field, necessity)) = left.next() {
                    loop {
                        let Some((other, lower_necessity)) = right.next() else {
                            return false;
                        };

                        if field != other {
                            continue;
                        }

                        if lower_necessity > necessity {
                            return false;
                        }

                        break;
                    }
                }

                true
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        gaia::type_subst::{FunctorArguments, Necessity},
        lattice::PreOrder,
    };

    use super::Functor;

    #[test]
    fn preorder_functor() {
        let f_2 = Functor {
            tag: Some("f".into()),
            args: FunctorArguments::List { arity: 2 },
        };

        let g_2 = Functor {
            tag: Some("g".into()),
            args: FunctorArguments::List { arity: 2 },
        };

        let f_3 = Functor {
            tag: Some("f".into()),
            args: FunctorArguments::List { arity: 3 },
        };

        // f_2 <= f_2
        assert!(f_2.leq(&f_2));
        // f_2 || f_3
        assert!(!f_2.leq(&f_3));
        assert!(!f_3.leq(&f_2));
        // f_2 || g_2
        assert!(!f_2.leq(&g_2));
        assert!(!g_2.leq(&f_2));

        let f_name = Functor {
            tag: Some("f".into()),
            args: FunctorArguments::Map {
                keys: vec![("name".into(), Necessity::Required)],
            },
        };

        let f_age = Functor {
            tag: Some("f".into()),
            args: FunctorArguments::Map {
                keys: vec![("age".into(), Necessity::Required)],
            },
        };

        let f_oage = Functor {
            tag: Some("f".into()),
            args: FunctorArguments::Map {
                keys: vec![("age".into(), Necessity::Optional)],
            },
        };

        let f_oage_name = Functor {
            tag: Some("f".into()),
            args: FunctorArguments::Map {
                keys: vec![
                    ("age".into(), Necessity::Optional),
                    ("name".into(), Necessity::Required),
                ],
            },
        };

        // f_name || f_2
        assert!(!f_name.leq(&f_2));
        assert!(!f_2.leq(&f_name));
        // f_name || f_age
        assert!(!f_name.leq(&f_age));
        assert!(!f_age.leq(&f_name));
        // f_age < f_oage
    }
}
