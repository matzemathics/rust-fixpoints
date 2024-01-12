use crate::lattice::JoinSemiLattice;

use super::Variable;

pub trait AbstractSubstitution: JoinSemiLattice {
    type FunctionSymbol;

    fn apply_eq(&mut self, lhs: Variable, rhs: Variable);
    fn apply_func(&mut self, lhs: Variable, func: &Self::FunctionSymbol, rhs: &[Variable]);

    fn extended(&self, num_vars: u32) -> Self;
    fn restrict(&mut self, num_vars: u32);

    fn project(&self, vars: &[Variable]) -> Self;
    fn propagate(&mut self, vars: &[Variable], update: Self);

    fn len(&self) -> u32;
}
