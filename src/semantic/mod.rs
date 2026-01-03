//! Semantic analysis module for type checking and validation.

mod type_checker;
mod symbol_table;

pub use type_checker::TypeChecker;
pub use symbol_table::{Symbol, SymbolKind, SymbolTable, Scope, FieldDef, StructDef, EnumDef, TypeDef};
