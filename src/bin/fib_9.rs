use fixpoints::type_inference::fixpoint::compute_fixpoint;
use fixpoints::type_inference::fixpoint::MonotoneTransform;
use fixpoints::type_inference::fixpoint::Recursor;

pub(crate) struct Fibonacci;

impl MonotoneTransform<u64> for Fibonacci {
    type Output = u64;

    fn call(&self, mut f: impl Recursor<u64, u64>, a: u64) -> u64 {
        if a == 0 || a == 1 {
            1
        } else {
            let a_1 = *f.recurse(a - 1);
            let a_2 = *f.recurse(a - 2);
            a_1 + a_2
        }
    }
}

fn main() {
    let table = compute_fixpoint(9, Fibonacci);
    println!("Result {}", table.map.get(&9).unwrap());
}
