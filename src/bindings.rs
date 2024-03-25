use std::collections::HashMap;

use pyo3::{
    exceptions::PyTypeError,
    prelude::*,
    types::{PyDict, PySet},
};

use crate::{
    traits::lattice::Top,
    type_inference::{
        fixpoint::compute_fixpoint,
        model::{BodyAtom, BodyBuiltin, BodyTerm, PatClause},
        type_table::TypeTable,
        Program, TypeAnalysis,
    },
    type_terms::{
        const_model::{NemoCtor, NemoFunctor, NestedFunctor},
        flat_type::{IntType, WildcardType},
        structured_type::{OrNode, TypeNode},
        topped_lattice::ToppedLattice,
    },
};

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
#[derive(Clone)]
struct Rule {
    predicate: String,
    clause: PatClause<String, NemoFunctor, NemoCtor, NemoBuiltin>,
}

#[pyclass]
#[derive(Clone)]
struct BuiltinExpr(BodyBuiltin<NemoFunctor, NemoBuiltin>);

fn type_spec(py: Python<'_>, p: PyObject) -> PyResult<FlatType> {
    let builtins = PyModule::import(py, "builtins")?;
    if p.is(builtins.getattr("str")?) {
        return Ok(FlatType::str());
    }
    if p.is(builtins.getattr("int")?) {
        return Ok(FlatType::int());
    }

    Err(PyTypeError::new_err("invalid argument to import_data"))
}

#[pyfunction(signature = (*args))]
fn import_data(py: Python<'_>, args: Vec<PyObject>) -> PyResult<BuiltinExpr> {
    let terms = (0..args.len() as u16).map(BodyTerm::Var).collect();
    Ok(BuiltinExpr(BodyBuiltin {
        builtin: NemoBuiltin::Import(
            args.into_iter()
                .map(|t| type_spec(py, t))
                .collect::<Result<_, _>>()?,
        ),
        terms,
    }))
}

#[pyclass]
#[derive(Clone, Copy, Debug)]
struct Var(u16);

#[pyfunction]
fn var(v: u16) -> Var {
    Var(v)
}

#[pyclass]
#[derive(Debug, Clone, Copy)]
struct Wildcard;

#[pyfunction]
fn wildcard() -> Wildcard {
    Wildcard
}

fn term<T: TermLike>(py: Python<'_>, it: PyObject) -> PyResult<T> {
    if let Ok(num) = it.extract::<i64>(py) {
        return Ok(TermLike::constant(IdentConstant::IntConst(num)));
    }
    if let Ok(str) = it.extract::<String>(py) {
        return Ok(TermLike::constant(IdentConstant::StrConst(str.into())));
    }
    if let Ok(v) = it.extract::<Var>(py) {
        return Ok(TermLike::variable(v.0));
    }
    if let Ok(_) = it.extract::<Wildcard>(py) {
        return T::wildcard().ok_or(PyTypeError::new_err("cannot use wildcard in head"));
    }

    Err(PyTypeError::new_err("unexpected arguments"))
}

#[pyfunction]
fn atom(py: Python<'_>, predicate: String, terms: Vec<PyObject>) -> Atom {
    Atom { predicate, terms }
}

fn body_atom(py: Python<'_>, atom: Atom) -> PyResult<BodyAtom<NemoFunctor, String>> {
    Ok(BodyAtom {
        predicate: atom.predicate,
        terms: atom
            .terms
            .into_iter()
            .map(|t| term(py, t))
            .collect::<Result<_, _>>()?,
    })
}

fn max_variable(term: &BodyTerm<NemoFunctor>) -> u16 {
    match term {
        BodyTerm::Var(v) => *v,
        BodyTerm::Functor { subterms, .. } => subterms.iter().map(max_variable).max().unwrap_or(0),
        BodyTerm::DontCare => 0,
    }
}

#[pyfunction]
fn rule(py: Python<'_>, head: Atom, body: Vec<PyObject>) -> PyResult<Rule> {
    let builtins: Vec<BodyBuiltin<_, _>> = body
        .iter()
        .filter_map(|b| b.extract(py).ok().map(|b: BuiltinExpr| b.0))
        .collect();

    let body: Vec<_> = body
        .iter()
        .filter_map(|b| b.extract(py).ok().map(|a| body_atom(py, a)))
        .collect::<Result<_, _>>()?;

    let body_variables = body
        .iter()
        .flat_map(|atom| &atom.terms)
        .chain(builtins.iter().flat_map(|b| &b.terms))
        .map(max_variable)
        .max()
        .map(|v| v + 1)
        .unwrap_or(0);

    Ok(Rule {
        predicate: head.predicate,
        clause: PatClause {
            head: head
                .terms
                .into_iter()
                .map(|t| term(py, t))
                .collect::<Result<_, _>>()?,
            body_atoms: body,
            body_builtins: builtins,
            body_variables,
        },
    })
}

#[pyclass]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct AnyNode;

#[pyclass]
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Functor(String);

#[pyclass]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct PyStringType;

#[pyclass]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct PyIntType;

#[pyclass]
#[derive(Debug, Clone)]
struct Jungle {
    start: PyObject,
    rules: PyObject,
}

#[pymethods]
impl Jungle {
    fn start(&self) -> &PyObject {
        &self.start
    }

    fn rules(&self) -> &PyObject {
        &self.rules
    }
}

impl ToPyObject for Jungle {
    fn to_object(&self, py: Python<'_>) -> PyObject {
        self.to_owned().into_py(py)
    }
}

fn flat_type_to_py(py: Python<'_>, typ: FlatType) -> Vec<PyObject> {
    let mut result = Vec::new();

    let FlatType {
        string,
        iri,
        language_tagged_string,
        float,
        double,
        integer,
        true_const,
        false_const,
        null,
        other,
    } = typ;

    if string == ToppedLattice::top() {
        result.push(PyStringType.into_py(py));
    } else {
        for str in string.iter() {
            result.push(str.to_string().into_py(py));
        }
    }

    if integer == IntType::top() {
        result.push(PyIntType.into_py(py));
    } else {
        for int in integer.constants() {
            result.push(int.into_py(py));
        }
    }

    if true_const {
        result.push(true.into_py(py))
    }
    if false_const {
        result.push(false.into_py(py))
    }

    result
}

fn flat_node_to_py(py: Python<'_>, node: TypeNode<FlatType>) -> PyResult<PyObject> {
    match node {
        TypeNode::Any => Ok(AnyNode.into_py(py)),
        TypeNode::TypeNode(OrNode {
            flat_types,
            functors,
        }) => {
            let mut functors: Vec<_> = functors
                .into_iter()
                .map(|f| match f {
                    NestedFunctor::Map { tag, keys } => unimplemented!(),
                    NestedFunctor::List { tag, length } => {
                        Functor(tag.unwrap().to_string()).into_py(py)
                    }
                })
                .collect();

            functors.extend(flat_type_to_py(py, flat_types));
            Ok(PySet::new(py, &functors)?.into_py(py))
        }
    }
}

fn wildcard_type_to_py(py: Python<'_>, typ: WildcardType) -> Vec<PyObject> {
    let mut result = flat_type_to_py(py, typ.flat_type);
    if typ.wildcard {
        result.push(Wildcard.into_py(py));
    }
    result
}

fn wildcard_node_to_py(py: Python<'_>, node: TypeNode<WildcardType>) -> PyResult<PyObject> {
    match node {
        TypeNode::Any => Ok(AnyNode.into_py(py)),
        TypeNode::TypeNode(OrNode {
            flat_types,
            functors,
        }) => {
            let mut functors: Vec<_> = functors
                .into_iter()
                .map(|f| match f {
                    NestedFunctor::Map { tag, keys } => unimplemented!(),
                    NestedFunctor::List { tag, length } => {
                        Functor(tag.unwrap().to_string()).into_py(py)
                    }
                })
                .collect();

            functors.extend(wildcard_type_to_py(py, flat_types));
            Ok(PySet::new(py, &functors)?.into_py(py))
        }
    }
}

trait Jungable {
    fn make_jungle(self, py: Python<'_>) -> PyResult<Jungle>;
}

impl Jungable for StructuredType<FlatType> {
    fn make_jungle(self, py: Python<'_>) -> PyResult<Jungle> {
        let dict = PyDict::new(py);

        for (fun, nodes) in &self.grammar.rules {
            let nodes: Vec<_> = nodes
                .iter()
                .map(|node| flat_node_to_py(py, node.clone()))
                .collect::<Result<_, _>>()?;

            match fun {
                NestedFunctor::Map { tag, keys } => unimplemented!(),
                NestedFunctor::List { tag, length } => {
                    dict.set_item(tag.clone().unwrap().to_string(), nodes)?;
                }
            }
        }

        Ok(Jungle {
            start: flat_node_to_py(py, self.start)?,
            rules: dict.into_py(py),
        })
    }
}

impl Jungable for StructuredType<WildcardType> {
    fn make_jungle(self, py: Python<'_>) -> PyResult<Jungle> {
        let dict = PyDict::new(py);

        for (fun, nodes) in &self.grammar.rules {
            let nodes: Vec<_> = nodes
                .iter()
                .map(|node| wildcard_node_to_py(py, node.clone()))
                .collect::<Result<_, _>>()?;

            match fun {
                NestedFunctor::Map { tag, keys } => unimplemented!(),
                NestedFunctor::List { tag, length } => {
                    dict.set_item(tag.clone().unwrap().to_string(), nodes)?;
                }
            }
        }

        Ok(Jungle {
            start: wildcard_node_to_py(py, self.start)?,
            rules: dict.into_py(py),
        })
    }
}

fn mk_jungle_map(
    py: Python<'_>,
    map: &HashMap<String, TypeTable<impl Jungable + Clone>>,
) -> PyResult<PyObject> {
    let result = PyDict::new(py);
    for (pred, typ) in map {
        let multi_table: Vec<Vec<_>> = typ
            .rows
            .iter()
            .map(|tup| {
                tup.iter()
                    .cloned()
                    .map(|jungle| jungle.make_jungle(py))
                    .collect()
            })
            .collect::<Result<_, _>>()?;

        result.set_item(pred, multi_table)?;
    }

    Ok(result.into_py(py))
}

#[pyfunction]
fn inference(py: Python<'_>, rules: Vec<Rule>, output: String) -> PyResult<(PyObject, PyObject)> {
    let mut program = Program::new();

    for rule in rules {
        program.add_rule(rule.predicate, rule.clause);
    }

    let analysis: TypeAnalysis<_, StructuredType<FlatType>> = program.analyse();
    let fixpoint = compute_fixpoint(output.clone(), analysis.clone());
    let forward = mk_jungle_map(py, &fixpoint.map)?;

    let backward = analysis.backwards(fixpoint.deps, output, fixpoint.map);
    let backward = mk_jungle_map(py, &backward)?;

    Ok((forward, backward))
}

#[pymodule]
fn fixpoints(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(rule, m)?)?;
    m.add_function(wrap_pyfunction!(atom, m)?)?;
    m.add_function(wrap_pyfunction!(var, m)?)?;
    m.add_function(wrap_pyfunction!(import_data, m)?)?;
    m.add_function(wrap_pyfunction!(inference, m)?)?;
    m.add_function(wrap_pyfunction!(wildcard, m)?)?;
    m.add_class::<Jungle>()?;

    Ok(())
}
