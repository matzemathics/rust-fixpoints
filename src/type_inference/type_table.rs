use std::{
    borrow::Cow,
    iter::repeat_with,
    ops::{Deref, DerefMut},
};

use crate::lattice::{Bottom, Meet, PreOrder, Top};

use super::{tup::Tup, BodyTerm, Cons, HeadTerm, Uncons};

/// Stores types that can occur at any given variable
/// position
pub struct TypeTable<T> {
    width: u16,
    rows: Vec<Tup<T>>,
}

impl<T: Top> TypeTable<T> {
    pub fn new(width: u16) -> TypeTable<T> {
        Self {
            width,
            rows: vec![repeat_with(T::top).take(width as usize).collect()],
        }
    }
}

impl<T> TypeTable<T> {
    pub fn empty() -> Self {
        Self {
            width: 0,
            rows: vec![],
        }
    }
}

impl<T: Clone + PreOrder> TypeTable<T> {
    fn add_row(&mut self, new_row: Tup<T>) {
        for row in &mut self.rows {
            if new_row.leq(&row) {
                return;
            }

            if row.leq(&new_row) {
                *row = new_row;
                return;
            }
        }

        // new_row is incomparable to all current rows
        self.rows.push(new_row)
    }

    fn add_row_borrowed(&mut self, new_row: &Tup<T>) {
        for row in &mut self.rows {
            if new_row.leq(&row) {
                return;
            }

            if row.leq(new_row) {
                *row = new_row.clone();
                return;
            }
        }

        // new_row is incomparable to all current rows
        self.rows.push(new_row.clone())
    }

    // updates self = self union other
    pub fn union_with(&mut self, other: &Self) {
        assert_eq!(self.width, other.width);
        for row in &other.rows {
            self.add_row_borrowed(row)
        }
    }
}

impl<T: Bottom + Clone + Meet> TypeTable<T> {
    // computes the meet of self with other
    // other[i] is unified with positions[i](self)
    // this might increase the number of rows quadratically
    pub(super) fn meet<F>(&self, other: &Self, positions: &[BodyTerm<F>]) -> Self
    where
        T: Uncons<F>,
    {
        debug_assert_eq!(other.width as usize, positions.len());
        let mut result = Self {
            width: self.width,
            rows: Vec::default(),
        };

        for row in &self.rows {
            for other_row in &other.rows {
                if let Some(new_row) = row.clone().unify(other_row, positions) {
                    result.add_row(new_row)
                }
            }
        }

        result
    }
}

impl<T: Clone> Tup<T> {
    fn interpret_head_term<C>(&self, t: &HeadTerm<C>) -> Option<T>
    where
        C: Clone,
        T: Cons<C>,
    {
        match t {
            HeadTerm::Var(v) => Some(self[*v as usize].clone()),
            HeadTerm::NemoCtor(c, subterms) => {
                let subterms = subterms
                    .iter()
                    .map(|term| self.interpret_head_term(term))
                    .collect::<Option<_>>()?;
                T::cons(c.clone(), subterms)
            }
        }
    }

    fn interpret_head_atom<C: Clone>(original: &Self, shape: &[HeadTerm<C>]) -> Option<Self>
    where
        T: Cons<C>,
    {
        shape
            .iter()
            .map(|term| original.interpret_head_term(term))
            .collect()
    }

    fn interpret_body_term<F>(&self, t: &BodyTerm<F>) -> Option<T>
    where
        F: Clone,
        T: Cons<F> + Top,
    {
        match t {
            BodyTerm::Var(v) => Some(self[*v as usize].clone()),
            BodyTerm::Functor { functor, subterms } => {
                let subterms = subterms
                    .iter()
                    .map(|t| self.interpret_body_term(t))
                    .collect::<Option<_>>()?;
                T::cons(functor.clone(), subterms)
            }
            BodyTerm::DontCare => Some(T::top()),
        }
    }

    fn interpret_body_atom<F>(&self, shape: &[BodyTerm<F>]) -> Option<Self>
    where
        F: Clone,
        T: Cons<F> + Top,
    {
        shape.iter().map(|t| self.interpret_body_term(t)).collect()
    }
}

impl<T> TypeTable<T> {
    pub(super) fn apply_ctor<Ctor>(self, shape: &[HeadTerm<Ctor>]) -> TypeTable<T>
    where
        Ctor: Clone,
        T: Cons<Ctor> + Clone,
    {
        let rows = self
            .rows
            .into_iter()
            .filter_map(|row| Tup::interpret_head_atom(&row, shape))
            .collect();

        TypeTable {
            width: shape.len() as u16,
            rows,
        }
    }
}

pub(super) trait InterpretBuiltin: Sized {
    type Builtin: Clone;

    fn interpret(builtin: Self::Builtin, tup: Tup<Self>) -> Option<Tup<Self>>;
}

impl<T> TypeTable<T> {
    pub(super) fn apply_builtin<F>(&mut self, builtin: &T::Builtin, shape: &[BodyTerm<F>])
    where
        F: Clone,
        T: Clone + Top + Cons<F> + Uncons<F> + InterpretBuiltin + Bottom + Meet,
    {
        self.rows.retain_mut(|row| {
            row.interpret_body_atom(shape)
                .and_then(|tup| T::interpret(builtin.clone(), tup))
                .and_then(|assignment| row.unify_with(&assignment, shape).then_some(()))
                .is_some()
        })
    }
}
