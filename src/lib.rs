//! CScript - A strongly statically typed superset of C
//!
//! CScript adds compile-time safety guarantees to C without runtime overhead.
//! It transpiles to clean, readable C code.

pub mod lexer;
pub mod parser;
pub mod semantic;
pub mod codegen;
pub mod diagnostics;
pub mod driver;

// Re-export commonly used types
pub use driver::Driver;
pub use diagnostics::{Diagnostic, DiagnosticLevel, SourceLocation};
