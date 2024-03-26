use std::{collections::HashMap, sync::Arc};

use pyo3::{
    exceptions::PyTypeError,
    prelude::*,
    types::{PyDict, PySet, PyString},
};

use crate::{
    traits::lattice::{Meet, Top},
    type_inference::{
        fixpoint::compute_fixpoint,
        model::{BodyAtom, BodyBuiltin, BodyTerm, PatClause},
        tup::UnificationFailure,
        type_table::TypeTable,
        Program, TypeAnalysis,
    },
    type_terms::{
        const_model::{NemoCtor, NemoFunctor, NestedFunctor},
        flat_type::{IntType, WildcardType},
        structured_type::{OrNode, StructuredTypeConfig, TypeNode},
        topped_lattice::ToppedLattice,
    },
    util::tup::Tup,
};

use crate::type_terms::{
    const_model::{IdentConstant, NemoBuiltin, TermLike},
    flat_type::FlatType,
    structured_type::StructuredType,
};

#[pyclass]
#[derive(Debug, Clone)]
struct Atom {
    #[pyo3(get)]
    predicate: String,
    #[pyo3(get)]
    terms: Vec<PyObject>,
}

#[pyclass]
#[derive(Debug, Clone)]
struct Rule {
    predicate: String,
    clause: PatClause<String, NemoFunctor, NemoCtor, NemoBuiltin>,
}

#[pymethods]
impl Rule {
    fn __repr__(&self) -> String {
        format!("{:?}", self)
    }
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

#[pyclass]
#[derive(Debug, Clone)]
struct FunctorTerm(Arc<str>, Vec<PyObject>);

#[pyfunction]
fn functor(function_symbol: String, subterms: Vec<PyObject>) -> FunctorTerm {
    FunctorTerm(function_symbol.into(), subterms)
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
    if let Ok(FunctorTerm(fn_sym, terms)) = it.extract(py) {
        let subterms = terms
            .into_iter()
            .map(|t| term(py, t))
            .collect::<PyResult<Vec<T>>>()?;

        return Ok(T::functor(fn_sym, subterms));
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

struct Body {
    builtins: Vec<BodyBuiltin<NemoFunctor, NemoBuiltin>>,
    atoms: Vec<BodyAtom<NemoFunctor, String>>,
    num_variables: u16,
}

fn mk_body(py: Python<'_>, input: Vec<PyObject>) -> PyResult<Body> {
    let builtins: Vec<BodyBuiltin<_, _>> = input
        .iter()
        .filter_map(|b| b.extract(py).ok().map(|b: BuiltinExpr| b.0))
        .collect();

    let atoms: Vec<_> = input
        .iter()
        .filter_map(|b| b.extract(py).ok().map(|a| body_atom(py, a)))
        .collect::<Result<_, _>>()?;

    let num_variables = atoms
        .iter()
        .flat_map(|atom| &atom.terms)
        .chain(builtins.iter().flat_map(|b| &b.terms))
        .map(max_variable)
        .max()
        .map(|v| v + 1)
        .unwrap_or(0);

    Ok(Body {
        builtins,
        atoms,
        num_variables,
    })
}

#[pyfunction]
fn rule(py: Python<'_>, head: Atom, body: Vec<PyObject>) -> PyResult<Rule> {
    let Body {
        atoms,
        builtins,
        num_variables,
    } = mk_body(py, body)?;

    Ok(Rule {
        predicate: head.predicate,
        clause: PatClause {
            head: head
                .terms
                .into_iter()
                .map(|t| term(py, t))
                .collect::<Result<_, _>>()?,
            body_atoms: atoms,
            body_builtins: builtins,
            body_variables: num_variables,
        },
    })
}

#[pyclass]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct AnyNode;

#[pyclass]
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Functor(String);

#[pymethods]
impl Functor {
    fn __repr__(&self) -> String {
        format!("{}(...)", self.0)
    }
}

#[pyclass]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct PyStringType;

#[pymethods]
impl PyStringType {
    fn __repr__(&self) -> String {
        "str".into()
    }
}

#[pyclass]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct PyIntType;

#[pymethods]
impl PyIntType {
    fn __repr__(&self) -> String {
        "int".into()
    }
}

#[pyclass]
#[derive(Debug, Clone)]
struct Jungle {
    #[pyo3(get)]
    start: PyObject,
    #[pyo3(get)]
    rules: PyObject,
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

#[pyclass]
#[derive(Debug, Clone)]
struct FwTyp(StructuredType<FlatType>);

#[pymethods]
impl FwTyp {
    fn __repr__(&self) -> String {
        format!("{:?}", self.0)
    }

    fn jungle(&self, py: Python<'_>) -> PyResult<Jungle> {
        let dict = PyDict::new(py);

        for (fun, nodes) in &self.0.grammar.rules {
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
            start: flat_node_to_py(py, self.0.start.clone())?,
            rules: dict.into_py(py),
        })
    }
}

#[pyclass]
#[derive(Debug, Clone)]
struct BwTyp(StructuredType<WildcardType>);

#[pymethods]
impl BwTyp {
    fn __repr__(&self) -> String {
        format!("{:?}", self.0)
    }

    fn jungle(&self, py: Python<'_>) -> PyResult<Jungle> {
        let dict = PyDict::new(py);

        for (fun, nodes) in &self.0.grammar.rules {
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
            start: wildcard_node_to_py(py, self.0.start.clone())?,
            rules: dict.into_py(py),
        })
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

type ResultMap<T> = HashMap<String, Vec<Vec<T>>>;

#[pyfunction]
fn inference(rules: Vec<Rule>, output: String) -> PyResult<(ResultMap<FwTyp>, ResultMap<BwTyp>)> {
    let mut program = Program::new();

    for rule in rules {
        program.add_rule(rule.predicate, rule.clause);
    }

    let analysis: TypeAnalysis<_, StructuredType<FlatType>> = program.analyse();
    let fixpoint = compute_fixpoint(output.clone(), analysis.clone());
    let forward = fixpoint
        .map
        .iter()
        .map(|(k, v)| {
            (
                k.clone(),
                v.rows
                    .iter()
                    .map(|tup| tup.iter().cloned().map(FwTyp).collect())
                    .collect(),
            )
        })
        .collect();

    let backward = analysis.backwards(fixpoint.deps, output, fixpoint.map);
    let backward = backward
        .iter()
        .map(|(k, v)| {
            (
                k.clone(),
                v.rows
                    .iter()
                    .map(|tup| tup.iter().cloned().map(BwTyp).collect())
                    .collect(),
            )
        })
        .collect();

    Ok((forward, backward))
}

#[pyclass]
#[derive(Debug, Clone)]
struct MeetFailure {
    #[pyo3(get)]
    position: usize,
    #[pyo3(get)]
    predicate: String,
    #[pyo3(get)]
    variable: u16,
    #[pyo3(get)]
    lhs: FwTyp,
    #[pyo3(get)]
    rhs: FwTyp,
    #[pyo3(get)]
    path: Vec<usize>,
}

#[pymethods]
impl MeetFailure {
    fn __repr__(&self) -> String {
        format!("{:?}", self)
    }
}

#[pyclass]
#[derive(Debug, Clone)]
struct DeconstructFailure {
    #[pyo3(get)]
    position: usize,
    #[pyo3(get)]
    predicate: String,
    #[pyo3(get)]
    inferred: FwTyp,
    #[pyo3(get)]
    pattern: PyObject,
    #[pyo3(get)]
    path: Vec<usize>,
}

#[pymethods]
impl DeconstructFailure {
    fn __repr__(&self) -> String {
        format!("{:?}", self)
    }
}

enum Failure {
    Meet(MeetFailure),
    Deconstruct(DeconstructFailure),
}

fn exec_body(
    py: Python<'_>,
    fw: ResultMap<FwTyp>,
    body: Body,
) -> Result<TypeTable<StructuredType<FlatType>>, Vec<Failure>> {
    let mut table: TypeTable<_> = TypeTable::new(body.num_variables);

    for (body_position, BodyAtom { predicate, terms }) in body.atoms.iter().enumerate() {
        let other = TypeTable {
            rows: fw
                .get(predicate)
                .unwrap()
                .into_iter()
                .map(|tup| Tup(tup.iter().map(|FwTyp(t)| t.clone()).collect()))
                .collect(),
        };

        table = match table.meet(&other, terms.as_slice()) {
            Ok(table) => table,
            Err(failures) => {
                return Err(failures
                    .into_iter()
                    .map(|f| match f {
                        UnificationFailure::Incomparable {
                            variable,
                            path,
                            left,
                            right,
                        } => Failure::Meet(MeetFailure {
                            position: body_position,
                            predicate: predicate.clone(),
                            variable,
                            lhs: FwTyp(left),
                            rhs: FwTyp(right),
                            path,
                        }),
                        UnificationFailure::Deconstruct { path, typ, pattern } => {
                            Failure::Deconstruct(DeconstructFailure {
                                position: body_position,
                                predicate: predicate.clone(),
                                inferred: FwTyp(typ),
                                pattern: match pattern {
                                    NemoFunctor::Nested(NestedFunctor::List { tag, .. }) => {
                                        Functor(tag.unwrap().to_string()).into_py(py)
                                    }
                                    NemoFunctor::Const(IdentConstant::IntConst(num)) => {
                                        num.into_py(py)
                                    }
                                    NemoFunctor::Const(IdentConstant::StrConst(sconst)) => {
                                        sconst.into_py(py)
                                    }
                                    _ => unimplemented!(),
                                },
                                path,
                            })
                        }
                    })
                    .collect())
            }
        }
    }

    for BodyBuiltin { builtin, terms } in &body.builtins {
        table.apply_builtin(&StructuredTypeConfig {}, builtin, terms);
    }

    Ok(table)
}

#[pyfunction]
fn execute_body(
    py: Python<'_>,
    typ_result: ResultMap<FwTyp>,
    body: Vec<PyObject>,
) -> PyResult<(Vec<Vec<FwTyp>>, Vec<PyObject>)> {
    let body = mk_body(py, body)?;

    let table = match exec_body(py, typ_result, body) {
        Ok(table) => table,
        Err(failures) => {
            let failures = failures
                .into_iter()
                .map(|f| match f {
                    Failure::Meet(inner) => inner.into_py(py),
                    Failure::Deconstruct(inner) => inner.into_py(py),
                })
                .collect();
            return Ok((Vec::new(), failures));
        }
    };

    let res: Vec<Vec<_>> = table
        .rows
        .into_iter()
        .map(|tup| tup.iter().cloned().map(FwTyp).collect())
        .collect();

    Ok((res, Vec::new()))
}

#[pymodule]
fn fixpoints(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(rule, m)?)?;
    m.add_function(wrap_pyfunction!(atom, m)?)?;
    m.add_function(wrap_pyfunction!(var, m)?)?;
    m.add_function(wrap_pyfunction!(functor, m)?)?;
    m.add_function(wrap_pyfunction!(import_data, m)?)?;
    m.add_function(wrap_pyfunction!(inference, m)?)?;
    m.add_function(wrap_pyfunction!(wildcard, m)?)?;
    m.add_function(wrap_pyfunction!(execute_body, m)?)?;
    m.add_class::<Jungle>()?;
    m.add_class::<FwTyp>()?;
    m.add_class::<BwTyp>()?;
    m.add_class::<DeconstructFailure>()?;
    m.add_class::<MeetFailure>()?;

    Ok(())
}
