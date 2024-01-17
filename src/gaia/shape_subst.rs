use std::{collections::VecDeque, sync::Arc};

use crate::lattice::{JoinSemiLattice, PreOrder};

use super::abstract_substitution::AbstractSubstitution;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Substitution<AD>(Vec<AD>);

impl<AD> Substitution<AD> {
    fn len(&self) -> u32 {
        self.0.len() as u32
    }
}

#[cfg(old_subst)]
impl<AD: PartialOrd> PartialOrd for Substitution<AD> {
    fn le(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }

        self.0.iter().zip(&other.0).all(|(a, b)| a.le(b))
    }

    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self == other {
            Some(Ordering::Equal)
        } else {
            if self <= other {
                if other <= self {
                    None
                } else {
                    Some(Ordering::Less)
                }
            } else {
                if other <= self {
                    Some(Ordering::Greater)
                } else {
                    None
                }
            }
        }
    }
}

#[cfg(old_subst)]
impl<AD: JoinSemiLattice> JoinSemiLattice for Substitution<AD> {
    fn bot() -> Self {
        Substitution(Vec::new())
    }

    fn join(&self, other: &Self) -> Self {
        Substitution(
            (0..std::cmp::max(self.0.len(), other.0.len()))
                .map(|i| AD::join_opt(self.0.get(i), other.0.get(i)))
                .collect(),
        )
    }
}

#[cfg(old_subst)]
impl<AD: AbstractDomain + Clone> Substitution<AD> {
    fn extended(&self, num_vars: u32) -> Self {
        let mut res = Vec::with_capacity(num_vars as usize);
        res.extend_from_slice(&self.0);
        res.resize_with(num_vars as usize, AD::bot);
        Substitution(res)
    }

    fn restrict(&mut self, num_vars: u32) {
        self.0.resize_with(num_vars as usize, || {
            panic!("restrict: num_vars must be less then length")
        });
    }

    fn rename_project(&self, vars: &[Variable]) -> Self {
        Substitution(vars.iter().map(|&v| self.0[v as usize].clone()).collect())
    }

    fn extend_renamed(&mut self, vars: &[Variable], updated: Self) {
        for (var, update) in vars.iter().zip(updated.0) {
            self.0[*var as usize].unify_with(update);
        }
    }

    fn apply_eq_constraint(&mut self, lhs: Variable, rhs: Variable) {
        if lhs == rhs {
            return;
        }

        let higher = std::cmp::max(lhs, rhs) as usize;
        let lower = std::cmp::min(lhs, rhs) as usize;

        let (left, right) = self.0.split_at_mut(higher);

        left[lower].unify_replace_both(&mut right[0]);
    }

    fn apply_term_constraint(&mut self, lhs: Variable, rhs: AD::Term) {
        let (left, right) = self.0.split_at_mut(lhs as usize);
        let (current, right) = right.split_first_mut().unwrap();

        let subterms: Vec<_> = rhs
            .variables
            .iter()
            .map(|v| match *v {
                x if (x > lhs) => Some(&right[(x - lhs - 1) as usize]),
                x if (x == lhs) => None,
                _ => Some(&left[*v as usize]),
            })
            .collect();

        current.unify_nested(&rhs.tag, &subterms);
    }
}

#[derive(Debug, Clone)]
struct Pattern<F, D> {
    outputs: u16,
    nodes: Vec<PatNode<F>>,
    inputs: Vec<D>,
}

#[derive(Debug, Clone)]
enum PatNode<F> {
    Input(u16),
    Pattern { func: F, args: Vec<u16> },
}

impl<F, D> Pattern<F, D> {
    pub fn from_inputs(inputs: Vec<D>) -> Self {
        let len = inputs.len().try_into().expect("To many input variables");
        let nodes = (0..len).map(PatNode::Input).collect();

        Pattern {
            nodes,
            inputs,
            outputs: len,
        }
    }
}

impl<F: Clone, D: Structural<F> + Clone> Pattern<F, D> {
    pub fn collect_at(&self, index: u16) -> D {
        match &self.nodes[index as usize] {
            PatNode::Input(i) => self.inputs[*i as usize].clone(),
            PatNode::Pattern { func, args } => {
                D::cons(func.clone(), args.iter().map(|&i| self.collect_at(i)))
            }
        }
    }
}

trait Structural<F>: Sized {
    fn cons(func: F, args: impl Iterator<Item = Self>) -> Self;
    fn uncons(&self, func: &F) -> Vec<Self>;
    fn is_principal(&self, func: &F) -> bool;
}

pub trait Shape: Sized {
    fn arity(&self) -> u16;

    // fn meet(&self, other: &Self) -> Option<Self>;
    fn sub_shape(&self, other: &Self) -> Option<Vec<(u16, u16)>>;

    fn join(&self, other: &Self) -> Option<Self>;
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct SimpleFunctor {
    tag: Arc<str>,
    arity: u16,
}

impl Shape for SimpleFunctor {
    fn sub_shape(&self, other: &Self) -> Option<Vec<(u16, u16)>> {
        if self.tag != other.tag || self.arity != other.arity {
            return None;
        }

        Some((0..self.arity).map(|i| (i, i)).collect())
    }

    fn arity(&self) -> u16 {
        self.arity
    }

    fn join(&self, _: &Self) -> Option<Self> {
        None
    }
}

impl<F, D> PreOrder for Pattern<F, D>
where
    F: Shape,
    D: PreOrder + Structural<F> + Clone,
{
    fn leq(&self, other: &Self) -> bool {
        if self.outputs == 0 {
            return true;
        }

        if self.outputs != other.outputs {
            return false;
        }

        let mut left_constraints = Vec::new();
        let mut right_constraints = Vec::new();

        let mut subterm_constraints: VecDeque<_> = (0..self.outputs).map(|i| (i, i)).collect();

        while let Some((left, right)) = subterm_constraints.pop_front() {
            match &self.nodes[left as usize] {
                PatNode::Input(input) => match &other.nodes[right as usize] {
                    PatNode::Input(other_input) => {
                        if !self.inputs[*input as usize].leq(&other.inputs[*other_input as usize]) {
                            return false;
                        }
                    }
                    PatNode::Pattern { func, args } => {
                        let lower = &self.inputs[*input as usize];
                        if !lower.is_principal(func) {
                            return false;
                        }

                        for (lower, index) in lower.uncons(func).into_iter().zip(args) {
                            left_constraints.push((lower, *index))
                        }
                    }
                },
                PatNode::Pattern { func, args } => match &other.nodes[right as usize] {
                    PatNode::Input(other_input) => {
                        for (index, upper) in args
                            .iter()
                            .zip(other.inputs[*other_input as usize].uncons(func))
                        {
                            right_constraints.push((*index, upper))
                        }
                    }
                    PatNode::Pattern {
                        func: other_func,
                        args: other_args,
                    } => {
                        let Some(matching) = other_func.sub_shape(func) else {
                            return false;
                        };

                        for (left, right) in matching {
                            subterm_constraints
                                .push_back((args[right as usize], other_args[left as usize]))
                        }
                    }
                },
            }
        }

        while let Some((lower, index)) = left_constraints.pop() {
            match &other.nodes[index as usize] {
                PatNode::Input(input) => {
                    if !lower.leq(&other.inputs[*input as usize]) {
                        return false;
                    }
                }
                PatNode::Pattern { func, args } => {
                    if !lower.is_principal(func) {
                        return false;
                    }

                    for (lower, index) in lower.uncons(func).into_iter().zip(args) {
                        left_constraints.push((lower, *index))
                    }
                }
            }
        }

        while let Some((index, upper)) = right_constraints.pop() {
            match &self.nodes[index as usize] {
                PatNode::Input(input) => {
                    if !self.inputs[*input as usize].leq(&upper) {
                        return false;
                    }
                }
                PatNode::Pattern { func, args } => {
                    for (index, upper) in args.iter().zip(upper.uncons(func)) {
                        right_constraints.push((*index, upper))
                    }
                }
            }
        }

        return true;
    }
}

impl<F, D> JoinSemiLattice for Pattern<F, D>
where
    F: Shape + Clone,
    D: JoinSemiLattice + Structural<F> + Clone,
{
    fn bot() -> Self {
        Pattern {
            outputs: 0,
            nodes: Vec::default(),
            inputs: Vec::default(),
        }
    }

    fn join(&self, other: &Self) -> Self {
        if self.outputs == 0 {
            return other.clone();
        }
        if other.outputs == 0 {
            return self.clone();
        }

        if self.outputs != other.outputs {
            panic!("join: arity mismatch")
        }

        let mut inputs = Vec::new();
        let mut nodes = Vec::new();

        let mut subterm_pairs: VecDeque<_> =
            (0..self.outputs).map(|i| (Some(i), Some(i))).collect();

        let mut subterm_index = 0;

        while let Some((left, right)) = subterm_pairs.pop_front() {
            if left.is_none() {
                let idx = inputs.len();
                inputs.push(other.collect_at(right.unwrap()));
                nodes.push(PatNode::Input(idx as u16));
            }
            if right.is_none() {
                let idx = inputs.len();
                inputs.push(self.collect_at(left.unwrap()));
                nodes.push(PatNode::Input(idx as u16));
            }
            let left = left.unwrap();
            let right = right.unwrap();
            match (&self.nodes[left as usize], &other.nodes[right as usize]) {
                (PatNode::Input(lhs), PatNode::Input(rhs)) => {
                    let idx = inputs.len();
                    inputs.push(self.inputs[*lhs as usize].join(&other.inputs[*rhs as usize]));
                    nodes.push(PatNode::Input(idx as u16));
                }
                (
                    PatNode::Pattern {
                        func: lhs_f,
                        args: lhs_args,
                    },
                    PatNode::Pattern {
                        func: rhs_f,
                        args: rhs_args,
                    },
                ) => {
                    if let Some(func) = lhs_f.join(rhs_f) {
                        let mut left_sub =
                            func.sub_shape(lhs_f).expect("subshape of join").into_iter();
                        let mut right_sub =
                            func.sub_shape(rhs_f).expect("subshape of join").into_iter();

                        let mut left = left_sub.next();
                        let mut right = right_sub.next();

                        let mut args = Vec::new();

                        for i in 0..func.arity() {
                            match left {
                                Some(p) if p.0 == i => {
                                    left = left_sub.next();
                                    match right {
                                        Some(q) if q.0 == i => {
                                            right = right_sub.next();
                                            if lhs_args[p.1 as usize] == rhs_args[q.1 as usize] {
                                                args.push(lhs_args[p.1 as usize]);
                                            } else {
                                                let offset = subterm_pairs.len();
                                                subterm_pairs.push_back((
                                                    Some(lhs_args[p.1 as usize]),
                                                    Some(rhs_args[q.1 as usize]),
                                                ));
                                                args.push(subterm_index + offset as u16);
                                            }
                                        }
                                        _ => {
                                            let offset = subterm_pairs.len();
                                            subterm_pairs
                                                .push_back((Some(lhs_args[p.1 as usize]), None));
                                            args.push(subterm_index + offset as u16);
                                        }
                                    }
                                }
                                _ => {
                                    assert!(right.is_some() && right.unwrap().0 == i);
                                    let offset = subterm_pairs.len();
                                    subterm_pairs.push_back((
                                        None,
                                        Some(rhs_args[right.unwrap().1 as usize]),
                                    ));
                                    args.push(subterm_index + offset as u16);
                                    right = right_sub.next();
                                }
                            }
                        }

                        nodes.push(PatNode::Pattern { func, args })
                    } else {
                        let lhs = self.collect_at(left);
                        let rhs = other.collect_at(right);
                        let idx = inputs.len();
                        inputs.push(lhs.join(&rhs));
                        nodes.push(PatNode::Input(idx as u16));
                    }
                }
                _ => {
                    let lhs = self.collect_at(left);
                    let rhs = other.collect_at(right);
                    let idx = inputs.len();
                    inputs.push(lhs.join(&rhs));
                    nodes.push(PatNode::Input(idx as u16));
                }
            }

            subterm_index += 1;
        }
        todo!()
    }
}

impl<F, D> AbstractSubstitution<F> for Pattern<F, D>
where
    F: Shape + Clone,
    D: JoinSemiLattice + Structural<F> + Clone,
{
    fn apply_eq(&mut self, lhs: super::Variable, rhs: super::Variable) {
        todo!()
    }

    fn apply_func(&mut self, lhs: super::Variable, func: &F, rhs: &[super::Variable]) {
        todo!()
    }

    fn extended(&self, num_vars: u16) -> Self {
        todo!()
    }

    fn restrict(&mut self, num_vars: u16) {
        todo!()
    }

    fn project(&self, vars: &[super::Variable]) -> Self {
        todo!()
    }

    fn propagate(&mut self, vars: &[super::Variable], update: Self) {
        todo!()
    }

    fn len(&self) -> u16 {
        todo!()
    }
}
