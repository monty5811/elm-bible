//! Python bindings, built by maturin as `elm_bible_rs._elm_bible_rs` and re-exported
//! from the `elm-bible-rs` package. The typed API is in python/elm_bible_rs/_elm_bible_rs.pyi.

use pyo3::create_exception;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::PyTuple;

use crate::{Book, Error, Reference};

create_exception!(
    elm_bible_rs,
    InvalidReference,
    PyValueError,
    "The text or encoded pair is not a valid reference. str(err) is the Elm \
     package's message; err.code is a stable name for the kind of error."
);

fn to_py_err(py: Python<'_>, error: Error) -> PyErr {
    let err = InvalidReference::new_err(error.to_string());
    if let Err(e) = err.value(py).setattr("code", error.code()) {
        return e;
    }
    err
}

#[pymethods]
impl Book {
    /// All 66 books, in canonical order.
    #[staticmethod]
    #[pyo3(name = "all")]
    fn py_all(py: Python<'_>) -> PyResult<Bound<'_, PyTuple>> {
        PyTuple::new(py, Book::all().iter().copied())
    }

    /// The book with this number (1 = Genesis, 66 = Revelation). Raises
    /// ValueError for any other number.
    #[staticmethod]
    #[pyo3(name = "from_number")]
    fn py_from_number(number: i64) -> PyResult<Book> {
        Book::from_number(number)
            .ok_or_else(|| PyValueError::new_err(format!("no book number {number}")))
    }

    #[getter]
    #[pyo3(name = "number")]
    fn py_number(&self) -> u8 {
        self.number()
    }

    /// The name used in references: "Genesis", "Psalm".
    #[getter]
    #[pyo3(name = "name")]
    fn py_name(&self) -> &'static str {
        self.name()
    }

    /// The book's own name, for headings: "Genesis", "Psalms".
    #[getter]
    #[pyo3(name = "title")]
    fn py_title(&self) -> &'static str {
        self.title()
    }

    #[getter]
    #[pyo3(name = "chapters")]
    fn py_chapters(&self) -> u16 {
        self.chapters()
    }

    /// Verses in each chapter; index 0 is chapter 1.
    #[getter]
    #[pyo3(name = "verse_counts")]
    fn py_verse_counts<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        PyTuple::new(py, self.verse_counts())
    }

    /// Verses in the chapter, or 0 when there is no such chapter.
    #[pyo3(name = "verses")]
    fn py_verses(&self, chapter: i64) -> u16 {
        self.verses(chapter)
    }

    /// "OT" or "NT".
    #[getter]
    fn testament(&self) -> &'static str {
        if self.is_old_testament() { "OT" } else { "NT" }
    }

    /// The lowercase spellings the parser accepts, e.g. ("genesis", "gen").
    #[getter]
    #[pyo3(name = "aliases")]
    fn py_aliases<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyTuple>> {
        PyTuple::new(py, self.aliases().collect::<Vec<_>>())
    }

    fn __str__(&self) -> &'static str {
        self.title()
    }

    /// The hash of the book's number, since a book equals its number
    /// (`Book.Genesis == 1`), as with `IntEnum`.
    fn __hash__(&self) -> u64 {
        u64::from(self.number())
    }
}

/// A validated passage. Make one with `parse()` or `decode()`.
#[pyclass(name = "Reference", module = "elm_bible_rs", frozen, eq, hash)]
#[derive(PartialEq, Eq, Hash)]
struct PyReference(Reference);

#[pymethods]
impl PyReference {
    /// Parse text such as "Gen 1:1-5". Raises InvalidReference.
    #[staticmethod]
    fn parse(py: Python<'_>, text: &str) -> PyResult<Self> {
        Reference::parse(text)
            .map(PyReference)
            .map_err(|e| to_py_err(py, e))
    }

    /// Build a reference from its (start, end) encoding. Raises InvalidReference.
    #[staticmethod]
    fn decode(py: Python<'_>, start: i64, end: i64) -> PyResult<Self> {
        Reference::decode(start, end)
            .map(PyReference)
            .map_err(|e| to_py_err(py, e))
    }

    /// The encoded first verse: book * 1_000_000 + chapter * 1_000 + verse.
    #[getter]
    fn start(&self) -> i64 {
        self.0.start()
    }

    /// The encoded last verse.
    #[getter]
    fn end(&self) -> i64 {
        self.0.end()
    }

    /// (start, end)
    fn encode(&self) -> (i64, i64) {
        self.0.encode()
    }

    #[getter]
    fn start_book(&self) -> Book {
        self.0.start_book()
    }
    #[getter]
    fn start_chapter(&self) -> i32 {
        self.0.start_chapter()
    }
    #[getter]
    fn start_verse(&self) -> i32 {
        self.0.start_verse()
    }
    #[getter]
    fn end_book(&self) -> Book {
        self.0.end_book()
    }
    #[getter]
    fn end_chapter(&self) -> i32 {
        self.0.end_chapter()
    }
    #[getter]
    fn end_verse(&self) -> i32 {
        self.0.end_verse()
    }

    /// The canonical text, e.g. "Genesis 1:1-5". Same as str(ref).
    fn format(&self) -> String {
        self.0.to_string()
    }

    fn __str__(&self) -> String {
        self.0.to_string()
    }

    fn __repr__(&self) -> String {
        format!(
            "<Reference {} ({}, {})>",
            self.0,
            self.0.start(),
            self.0.end()
        )
    }
}

/// Parse text such as "Gen 1:1-5". Raises InvalidReference.
#[pyfunction]
fn parse(py: Python<'_>, text: &str) -> PyResult<PyReference> {
    PyReference::parse(py, text)
}

/// Build a reference from its (start, end) encoding. Raises InvalidReference.
#[pyfunction]
fn decode(py: Python<'_>, start: i64, end: i64) -> PyResult<PyReference> {
    PyReference::decode(py, start, end)
}

/// The canonical text of an encoded (start, end) pair. Raises InvalidReference.
#[pyfunction]
fn format(py: Python<'_>, start: i64, end: i64) -> PyResult<String> {
    crate::format(start, end).map_err(|e| to_py_err(py, e))
}

#[pymodule]
fn _elm_bible_rs(m: &Bound<'_, PyModule>) -> PyResult<()> {
    let py = m.py();
    m.add_class::<Book>()?;
    m.add_class::<PyReference>()?;
    m.add("InvalidReference", py.get_type::<InvalidReference>())?;
    m.add_function(wrap_pyfunction!(parse, m)?)?;
    m.add_function(wrap_pyfunction!(decode, m)?)?;
    m.add_function(wrap_pyfunction!(format, m)?)?;
    m.add("BOOKS", PyTuple::new(py, Book::all().iter().copied())?)?;
    m.add("__version__", env!("CARGO_PKG_VERSION"))?;
    Ok(())
}
