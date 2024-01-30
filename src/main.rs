use lattice::Join;

use crate::fixpoint::compute_fixpoint;
// use crate::gaia::{default_substitution, Clause, Gaia, Query};

pub(crate) mod bitmap;
pub(crate) mod fixed_vec;
mod fixpoint;
mod lattice;
mod nemo_model;
mod type_inference;

fn main() {}
