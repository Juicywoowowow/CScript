//! Symbol table for tracking declarations and scopes.

use std::collections::HashMap;
use crate::parser::{TypeSpec, FunctionDecl};

/// Symbol table with nested scopes
#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
    /// Track all defined types (structs, enums, typedefs)
    types: HashMap<String, TypeDef>,
}

/// A single scope level
#[derive(Debug, Clone)]
pub struct Scope {
    symbols: HashMap<String, Symbol>,
    /// Name of the scope (for functions)
    pub name: Option<String>,
}

impl Scope {
    pub fn new(name: Option<String>) -> Self {
        Self {
            symbols: HashMap::new(),
            name,
        }
    }
}

/// A symbol in the symbol table
#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub type_spec: TypeSpec,
    pub is_mutable: bool,
    pub is_initialized: bool,
    pub is_used: bool,
    pub offset: usize,  // Source offset for error reporting
}

/// Kind of symbol
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    Variable,
    Parameter,
    Function,
    EnumVariant,
}

/// Type definition (struct, enum, typedef)
#[derive(Debug, Clone)]
pub enum TypeDef {
    Struct(StructDef),
    Enum(EnumDef),
    Union(UnionDef),
    Typedef(TypeSpec),
}

/// Struct definition
#[derive(Debug, Clone)]
pub struct StructDef {
    pub fields: Vec<FieldDef>,
}

/// Field definition
#[derive(Debug, Clone)]
pub struct FieldDef {
    pub name: String,
    pub type_spec: TypeSpec,
    pub is_mutable: bool,
}

/// Enum definition
#[derive(Debug, Clone)]
pub struct EnumDef {
    pub variants: Vec<String>,
}

/// Union definition
#[derive(Debug, Clone)]
pub struct UnionDef {
    pub fields: Vec<FieldDef>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new(None)], // Global scope
            types: HashMap::new(),
        }
    }

    /// Enter a new scope
    pub fn push_scope(&mut self, name: Option<String>) {
        self.scopes.push(Scope::new(name));
    }

    /// Exit current scope, returning unused symbols for warnings
    pub fn pop_scope(&mut self) -> Option<Vec<Symbol>> {
        if self.scopes.len() > 1 {
            let scope = self.scopes.pop()?;
            let unused: Vec<_> = scope.symbols
                .into_values()
                .filter(|s| !s.is_used && s.kind != SymbolKind::Function)
                .collect();
            Some(unused)
        } else {
            None
        }
    }

    /// Define a symbol in the current scope
    pub fn define(&mut self, symbol: Symbol) -> Result<(), Symbol> {
        let scope = self.scopes.last_mut().unwrap();
        
        if scope.symbols.contains_key(&symbol.name) {
            // Symbol already defined in this scope
            return Err(symbol);
        }
        
        scope.symbols.insert(symbol.name.clone(), symbol);
        Ok(())
    }

    /// Look up a symbol by name (searches all scopes from innermost to outermost)
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.symbols.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    /// Look up a symbol mutably
    pub fn lookup_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(symbol) = scope.symbols.get_mut(name) {
                return Some(symbol);
            }
        }
        None
    }

    /// Mark a symbol as used
    pub fn mark_used(&mut self, name: &str) {
        if let Some(symbol) = self.lookup_mut(name) {
            symbol.is_used = true;
        }
    }

    /// Check if a symbol is defined in the current scope only
    pub fn is_defined_in_current_scope(&self, name: &str) -> bool {
        self.scopes.last()
            .map(|s| s.symbols.contains_key(name))
            .unwrap_or(false)
    }

    /// Check if a symbol shadows one in an outer scope
    pub fn shadows(&self, name: &str) -> bool {
        // Skip current scope
        for scope in self.scopes.iter().rev().skip(1) {
            if scope.symbols.contains_key(name) {
                return true;
            }
        }
        false
    }

    /// Define a type
    pub fn define_type(&mut self, name: String, typedef: TypeDef) {
        self.types.insert(name, typedef);
    }

    /// Look up a type
    pub fn lookup_type(&self, name: &str) -> Option<&TypeDef> {
        self.types.get(name)
    }

    /// Get current scope depth (0 = global)
    pub fn depth(&self) -> usize {
        self.scopes.len() - 1
    }

    /// Check if we're in global scope
    pub fn is_global(&self) -> bool {
        self.scopes.len() == 1
    }

    /// Get current function name (if in a function)
    pub fn current_function(&self) -> Option<&str> {
        for scope in self.scopes.iter().rev() {
            if let Some(ref name) = scope.name {
                return Some(name);
            }
        }
        None
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}
