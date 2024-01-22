use crate::lattice::JoinSemiLattice;

use super::Variable;

pub trait AbstractSubstitution<F>: JoinSemiLattice {
    fn apply_eq(&mut self, lhs: Variable, rhs: Variable);
    fn apply_func(&mut self, lhs: Variable, func: &F, rhs: &[Variable]);

    fn extended(&self, num_vars: u16) -> Self;
    fn restrict(&mut self, num_vars: u16);

    fn project(&self, vars: &[Variable]) -> Self;
    fn propagate(&mut self, vars: &[Variable], update: Self);

    fn len(&self) -> u16;
}
