use pyo3::{exceptions::PyTypeError, prelude::*};

use crate::{type_inference::{
    fixpoint::compute_fixpoint,
    model::{BodyAtom, BodyBuiltin, BodyTerm, PatClause},
    Program,
}, type_terms::const_model::{NemoCtor, NemoFunctor}};

use crate::type_terms::{
    const_model::{IdentConstant, NemoBuiltin, TermLike},
    flat_type::FlatType,
    structured_type::StructuredType,
};

#[pyclass]
#[derive(Debug, Clone)]
struct Atom {
    predicate: String,
    terms: Vec<PyObject>,
}

#[pyclass]
struct Rule {
    predicate: String,
    clause: PatClause<String, NemoFunctor, NemoCtor, NemoBuiltin>
}

#[pyclass]
#[derive(Clone, Copy, Debug)]
struct Var(u16);

#[pyfunction]
fn var(v: u16) -> Var {
    Var(v)
}

fn term<T: TermLike>(py: Python<'_>, it: PyObject) -> PyResult<T> {
    if let Ok(num) = it.extract::<i64>(py) {
        return Ok(TermLike::constant(IdentConstant::IntConst(num)))
    }
    if let Ok(v) = it.extract::<Var>(py) {
        return Ok(TermLike::variable(v.0))
    }

    Err(PyTypeError::new_err("unexpected arguments"))
}

#[pyfunction]
fn atom(py: Python<'_>, predicate: String, terms: Vec<PyObject>) -> Atom {
    Atom {
        predicate,
        terms
    }
}

fn body_atom(py: Python<'_>, atom: Atom) -> PyResult<BodyAtom<NemoFunctor, String>> {
    Ok(BodyAtom {
        predicate: atom.predicate,
        terms: atom.terms.into_iter().map(|t| term(py, t)).collect::<Result<_, _>>()?
    })
}

#[pyfunction]
fn rule(py: Python<'_>, head: Atom, body: Vec<PyObject>) -> PyResult<Rule> {
    Ok(Rule {
        predicate: head.predicate,
        clause: PatClause {
            head: (),
            body_atoms: (),
            body_builtins: (),
            body_variables: ()
        }
    })
}