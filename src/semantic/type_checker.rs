//! Type checker for CScript semantic analysis.
//! Enforces CScript's stricter typing rules over C.

use super::{Symbol, SymbolKind, SymbolTable, FieldDef, StructDef, EnumDef, TypeDef, FunctionDef, ParamDef};
use crate::diagnostics::{codes, Diagnostic, DiagnosticReporter};
use crate::parser::*;

/// Type checker that validates the AST
pub struct TypeChecker<'a> {
    symbols: SymbolTable,
    reporter: &'a mut DiagnosticReporter,
    /// Current function return type (for return statement checking)
    current_return_type: Option<TypeSpec>,
    /// Track if we're inside a loop (for break/continue)
    loop_depth: usize,
}

impl<'a> TypeChecker<'a> {
    pub fn new(reporter: &'a mut DiagnosticReporter) -> Self {
        Self {
            symbols: SymbolTable::new(),
            reporter,
            current_return_type: None,
            loop_depth: 0,
        }
    }

    /// Check an entire program
    pub fn check(&mut self, program: &Program) {
        // First pass: collect all type definitions
        for decl in &program.declarations {
            self.collect_type_def(decl);
        }

        // Second pass: collect function signatures
        for decl in &program.declarations {
            if let Declaration::Function(func) = decl {
                self.declare_function(func);
            }
        }

        // Third pass: check all declarations
        for decl in &program.declarations {
            self.check_declaration(decl);
        }

        // Check for unused symbols in global scope
        // (We don't pop global scope, so check manually)
    }

    /// Collect type definitions (struct, enum, union, typedef)
    fn collect_type_def(&mut self, decl: &Declaration) {
        match decl {
            Declaration::Struct(s) => {
                if let Some(ref name) = s.name {
                    let fields: Vec<FieldDef> = s.fields.iter().map(|f| {
                        FieldDef {
                            name: f.name.clone(),
                            type_spec: f.type_spec.clone(),
                            is_mutable: f.is_mutable,
                        }
                    }).collect();
                    
                    self.symbols.define_type(
                        name.clone(),
                        TypeDef::Struct(StructDef { fields }),
                    );
                }
            }
            Declaration::Enum(e) => {
                if let Some(ref name) = e.name {
                    let variants: Vec<String> = e.variants.iter()
                        .map(|v| v.name.clone())
                        .collect();
                    
                    self.symbols.define_type(
                        name.clone(),
                        TypeDef::Enum(EnumDef { variants }),
                    );
                }
            }
            Declaration::Typedef(t) => {
                self.symbols.define_type(
                    t.new_name.clone(),
                    TypeDef::Typedef(t.original_type.clone()),
                );
            }
            _ => {}
        }
    }

    /// Declare a function signature (for forward references)
    fn declare_function(&mut self, func: &FunctionDecl) {
        let symbol = Symbol {
            name: func.name.clone(),
            kind: SymbolKind::Function,
            type_spec: func.return_type.clone(),
            is_mutable: false,
            is_initialized: true,
            is_used: false,
            offset: 0,
        };

        if let Err(_) = self.symbols.define(symbol) {
            // Function already declared - could be a redefinition error
            // but C allows multiple declarations, so we'll allow it
        }
    }

    /// Check a declaration
    fn check_declaration(&mut self, decl: &Declaration) {
        match decl {
            Declaration::Function(func) => self.check_function(func),
            Declaration::Variable(var) => self.check_variable(var, true),
            Declaration::Struct(s) => self.check_struct(s),
            Declaration::Enum(e) => self.check_enum(e),
            Declaration::Typedef(_) => {} // Already handled
            Declaration::Union(_) => {} // TODO: Check union
            Declaration::Preprocessor(_) => {} // Pass through
        }
    }

    /// Check a function declaration/definition
    fn check_function(&mut self, func: &FunctionDecl) {
        // Register function for argument validation (even for declarations)
        let func_def = FunctionDef {
            name: func.name.clone(),
            return_type: func.return_type.clone(),
            params: func.params.iter().map(|p| ParamDef {
                name: p.name.clone(),
                type_spec: p.type_spec.clone(),
            }).collect(),
        };
        self.symbols.define_function(func_def);
        
        if func.body.is_none() {
            return; // Just a declaration
        }

        self.symbols.push_scope(Some(func.name.clone()));
        self.current_return_type = Some(func.return_type.clone());

        // Add parameters to scope
        for param in &func.params {
            if let Some(ref name) = param.name {
                let symbol = Symbol {
                    name: name.clone(),
                    kind: SymbolKind::Parameter,
                    type_spec: param.type_spec.clone(),
                    is_mutable: param.is_mutable,
                    is_initialized: true,
                    is_used: false,
                    offset: 0,
                };

                if let Err(_) = self.symbols.define(symbol) {
                    self.reporter.add(
                        Diagnostic::error(codes::UNDEFINED_VARIABLE, 
                            format!("duplicate parameter name '{}'", name))
                    );
                }
            }
        }

        // Check function body
        if let Some(ref body) = func.body {
            self.check_block(body);
        }

        // Check for unused parameters
        if let Some(unused) = self.symbols.pop_scope() {
            for sym in unused {
                if sym.kind == SymbolKind::Parameter {
                    self.reporter.add(
                        Diagnostic::warning(codes::UNUSED_VARIABLE,
                            format!("unused parameter '{}'", sym.name))
                    );
                } else if sym.kind == SymbolKind::Variable {
                    self.reporter.add(
                        Diagnostic::warning(codes::UNUSED_VARIABLE,
                            format!("unused variable '{}'", sym.name))
                    );
                }
            }
        }

        self.current_return_type = None;
    }

    /// Check a variable declaration
    fn check_variable(&mut self, var: &VariableDecl, _is_global: bool) {
        // Check for void type
        if matches!(var.type_spec.base, BaseType::Void) && var.type_spec.pointer_depth == 0 {
            self.reporter.report_with_label(
                Diagnostic::error(codes::VOID_VARIABLE,
                    format!("variable '{}' declared as void", var.name))
                    .with_help("void can only be used as a pointer type (void*)"),
                var.name_span.offset,
                var.name_span.length,
                "declared as void here"
            );
        }

        // Check initializer type matches
        if let Some(ref init) = var.initializer {
            if let Some(init_type) = self.check_expr(init) {
                if !self.types_compatible(&var.type_spec, &init_type) {
                    self.reporter.report_with_label(
                        Diagnostic::error(codes::TYPE_MISMATCH,
                            format!("type mismatch in initialization of '{}'", var.name))
                            .with_help(format!("expected '{}', found '{}'",
                                self.type_to_string(&var.type_spec),
                                self.type_to_string(&init_type))),
                        var.name_span.offset,
                        var.name_span.length,
                        &format!("expected '{}'", self.type_to_string(&var.type_spec))
                    );
                } else if self.is_narrowing_conversion(&var.type_spec, &init_type) {
                    // Warn about narrowing conversion
                    self.reporter.report_with_label(
                        Diagnostic::warning(codes::NARROWING_CONVERSION,
                            format!("narrowing conversion in initialization of '{}'", var.name))
                            .with_help(format!("converting from '{}' to '{}' may lose data",
                                self.type_to_string(&init_type),
                                self.type_to_string(&var.type_spec))),
                        var.name_span.offset,
                        var.name_span.length,
                        "narrowing conversion here"
                    );
                }
            }
        } else if !var.is_mutable && !var.is_extern {
            // Immutable variables must be initialized
            self.reporter.report_with_label(
                Diagnostic::error(codes::UNINITIALIZED_VARIABLE,
                    format!("immutable variable '{}' must be initialized", var.name))
                    .with_help("add an initializer or mark as 'mut' for later assignment"),
                var.name_span.offset,
                var.name_span.length,
                "declared here without initializer"
            );
        }

        // Add to symbol table
        let symbol = Symbol {
            name: var.name.clone(),
            kind: SymbolKind::Variable,
            type_spec: var.type_spec.clone(),
            is_mutable: var.is_mutable,
            is_initialized: var.initializer.is_some(),
            is_used: false,
            offset: var.name_span.offset,  // Store the span offset for later error reporting
        };

        // Check for shadowing
        if self.symbols.shadows(&var.name) {
            self.reporter.report_with_label(
                Diagnostic::warning(codes::SHADOWED_VARIABLE,
                    format!("variable '{}' shadows a variable in an outer scope", var.name)),
                var.name_span.offset,
                var.name_span.length,
                "shadows outer variable"
            );
        }

        if let Err(_) = self.symbols.define(symbol) {
            self.reporter.report_with_label(
                Diagnostic::error(codes::UNDEFINED_VARIABLE,
                    format!("redefinition of '{}'", var.name)),
                var.name_span.offset,
                var.name_span.length,
                "already defined"
            );
        }
    }

    /// Check a struct definition
    fn check_struct(&mut self, s: &StructDecl) {
        for field in &s.fields {
            // Check for void fields
            if matches!(field.type_spec.base, BaseType::Void) && field.type_spec.pointer_depth == 0 {
                self.reporter.add(
                    Diagnostic::error(codes::VOID_VARIABLE,
                        format!("struct field '{}' cannot be void", field.name))
                );
            }
        }
    }

    /// Check an enum definition
    fn check_enum(&mut self, e: &EnumDecl) {
        // Add enum variants to global scope
        for variant in &e.variants {
            let symbol = Symbol {
                name: variant.name.clone(),
                kind: SymbolKind::EnumVariant,
                type_spec: TypeSpec {
                    base: BaseType::Int,
                    pointer_depth: 0,
                    is_const: true,
                    is_volatile: false,
                },
                is_mutable: false,
                is_initialized: true,
                is_used: false,
                offset: 0,
            };
            let _ = self.symbols.define(symbol);
        }
    }

    /// Check a block
    fn check_block(&mut self, block: &Block) {
        for stmt in &block.statements {
            self.check_stmt(stmt);
        }
    }

    /// Check a statement
    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression(expr) => {
                self.check_expr(expr);
            }

            Stmt::VarDecl(var) => {
                self.check_variable(var, false);
            }

            Stmt::Block(block) => {
                self.symbols.push_scope(None);
                self.check_block(block);
                if let Some(unused) = self.symbols.pop_scope() {
                    for sym in unused {
                        self.reporter.add(
                            Diagnostic::warning(codes::UNUSED_VARIABLE,
                                format!("unused variable '{}'", sym.name))
                        );
                    }
                }
            }

            Stmt::If { condition, then_branch, else_branch } => {
                if let Some(cond_type) = self.check_expr(condition) {
                    self.check_condition_type(&cond_type);
                }
                self.check_stmt(then_branch);
                if let Some(ref else_stmt) = else_branch {
                    self.check_stmt(else_stmt);
                }
            }

            Stmt::While { condition, body } => {
                if let Some(cond_type) = self.check_expr(condition) {
                    self.check_condition_type(&cond_type);
                }
                self.loop_depth += 1;
                self.check_stmt(body);
                self.loop_depth -= 1;
            }

            Stmt::DoWhile { body, condition } => {
                self.loop_depth += 1;
                self.check_stmt(body);
                self.loop_depth -= 1;
                if let Some(cond_type) = self.check_expr(condition) {
                    self.check_condition_type(&cond_type);
                }
            }

            Stmt::For { init, condition, update, body } => {
                self.symbols.push_scope(None);
                
                if let Some(ref init_stmt) = init {
                    self.check_stmt(init_stmt);
                }
                if let Some(ref cond) = condition {
                    if let Some(cond_type) = self.check_expr(cond) {
                        self.check_condition_type(&cond_type);
                    }
                }
                if let Some(ref upd) = update {
                    self.check_expr(upd);
                }
                
                self.loop_depth += 1;
                self.check_stmt(body);
                self.loop_depth -= 1;
                
                self.symbols.pop_scope();
            }

            Stmt::Switch { expr, cases, default } => {
                self.check_expr(expr);
                for case in cases {
                    self.check_expr(&case.value);
                    for stmt in &case.body {
                        self.check_stmt(stmt);
                    }
                }
                if let Some(ref default_block) = default {
                    self.check_block(default_block);
                }
            }

            Stmt::Match { expr, arms } => {
                // Verify expr is an Option type and extract inner type
                let inner_type = if let Some(expr_type) = self.check_expr(expr) {
                    if !expr_type.is_option() {
                        self.reporter.add(
                            Diagnostic::error(codes::TYPE_MISMATCH,
                                "match expression must be an Option type")
                        );
                        None
                    } else if let BaseType::Option(inner) = &expr_type.base {
                        Some(inner.as_ref().clone())
                    } else {
                        None
                    }
                } else {
                    None
                };

                for arm in arms {
                    self.symbols.push_scope(None);
                    
                    if let MatchPattern::Some(ref name) = arm.pattern {
                        // Bind the unwrapped value with the correct inner type
                        let bound_type = inner_type.clone().unwrap_or(TypeSpec {
                            base: BaseType::Void,
                            pointer_depth: 1,
                            is_const: false,
                            is_volatile: false,
                        });
                        
                        let symbol = Symbol {
                            name: name.clone(),
                            kind: SymbolKind::Variable,
                            type_spec: bound_type,
                            is_mutable: false,
                            is_initialized: true,
                            is_used: false,
                            offset: 0,
                        };
                        let _ = self.symbols.define(symbol);
                    }
                    
                    self.check_block(&arm.body);
                    self.symbols.pop_scope();
                }
            }

            Stmt::Return(expr) => {
                let ret_type = self.current_return_type.clone();
                if let Some(ref expected_ret_type) = ret_type {
                    let is_void = matches!(expected_ret_type.base, BaseType::Void);
                    let is_option = expected_ret_type.is_option();
                    
                    match (expr, is_void) {
                        (Some(e), false) => {
                            if let Some(expr_type) = self.check_expr(e) {
                                // Check for implicit Option wrapping
                                // If function returns Option<T> and we have T, that's OK
                                let compatible = if is_option {
                                    if let BaseType::Option(inner) = &expected_ret_type.base {
                                        // Allow both T and Option<T> as return values
                                        self.types_compatible(expected_ret_type, &expr_type) ||
                                        self.types_compatible(inner.as_ref(), &expr_type)
                                    } else {
                                        self.types_compatible(expected_ret_type, &expr_type)
                                    }
                                } else {
                                    self.types_compatible(expected_ret_type, &expr_type)
                                };
                                
                                if !compatible {
                                    self.reporter.add(
                                        Diagnostic::error(codes::RETURN_TYPE_MISMATCH,
                                            "return type mismatch")
                                            .with_help(format!("expected '{}', found '{}'",
                                                self.type_to_string(expected_ret_type),
                                                self.type_to_string(&expr_type)))
                                    );
                                }
                            }
                        }
                        (None, false) => {
                            self.reporter.add(
                                Diagnostic::error(codes::RETURN_TYPE_MISMATCH,
                                    "non-void function should return a value")
                            );
                        }
                        (Some(_), true) => {
                            self.reporter.add(
                                Diagnostic::error(codes::RETURN_TYPE_MISMATCH,
                                    "void function should not return a value")
                            );
                        }
                        (None, true) => {} // OK
                    }
                }
            }

            Stmt::Break => {
                if self.loop_depth == 0 {
                    self.reporter.add(
                        Diagnostic::error(codes::UNEXPECTED_TOKEN,
                            "'break' outside of loop")
                    );
                }
            }

            Stmt::Continue => {
                if self.loop_depth == 0 {
                    self.reporter.add(
                        Diagnostic::error(codes::UNEXPECTED_TOKEN,
                            "'continue' outside of loop")
                    );
                }
            }

            Stmt::Goto(_) | Stmt::Label(_) | Stmt::Empty => {}
            
            Stmt::StructDecl(s) => {
                // Register local struct type
                self.check_struct(s);
            }
        }
    }

    /// Check an expression and return its type
    fn check_expr(&mut self, expr: &Expr) -> Option<TypeSpec> {
        match expr {
            Expr::IntLiteral(_) => Some(TypeSpec {
                base: BaseType::Int,
                pointer_depth: 0,
                is_const: true,
                is_volatile: false,
            }),

            Expr::FloatLiteral(_) => Some(TypeSpec {
                base: BaseType::Double,
                pointer_depth: 0,
                is_const: true,
                is_volatile: false,
            }),

            Expr::StringLiteral(_) => Some(TypeSpec {
                base: BaseType::Char,
                pointer_depth: 1,
                is_const: true,
                is_volatile: false,
            }),

            Expr::CharLiteral(_) => Some(TypeSpec {
                base: BaseType::Char,
                pointer_depth: 0,
                is_const: true,
                is_volatile: false,
            }),

            Expr::BoolLiteral(_) => Some(TypeSpec {
                base: BaseType::Bool,
                pointer_depth: 0,
                is_const: true,
                is_volatile: false,
            }),

            Expr::None => Some(TypeSpec {
                base: BaseType::Void, // Special none type
                pointer_depth: 0,
                is_const: true,
                is_volatile: false,
            }),

            Expr::Identifier(name) => {
                let type_spec = self.symbols.lookup(name).map(|s| s.type_spec.clone());
                if let Some(ts) = type_spec {
                    self.symbols.mark_used(name);
                    Some(ts)
                } else {
                    self.reporter.add(
                        Diagnostic::error(codes::UNDEFINED_VARIABLE,
                            format!("undefined variable '{}'", name))
                    );
                    None
                }
            }

            Expr::Binary { left, op, right, span } => {
                let left_type = self.check_expr(left)?;
                let right_type = self.check_expr(right)?;
                
                // Check for valid operand types
                self.check_binary_types(&left_type, *op, &right_type, *span)
            }

            Expr::Unary { op, operand } => {
                let operand_type = self.check_expr(operand)?;
                self.check_unary_type(*op, &operand_type)
            }

            Expr::Assign { target, op: _, value, span } => {
                // Check mutability
                if let Expr::Identifier(name) = target.as_ref() {
                    if let Some(symbol) = self.symbols.lookup(name) {
                        if !symbol.is_mutable {
                            self.reporter.report_with_label(
                                Diagnostic::error(codes::MUTATE_IMMUTABLE,
                                    format!("cannot assign to immutable variable '{}'", name))
                                    .with_help("declare variable with 'mut' to allow assignment"),
                                span.offset,
                                span.length,
                                "assignment here"
                            );
                        }
                    }
                }

                let target_type = self.check_expr(target)?;
                let value_type = self.check_expr(value)?;

                if !self.types_compatible(&target_type, &value_type) {
                    self.reporter.report_with_label(
                        Diagnostic::error(codes::TYPE_MISMATCH,
                            "type mismatch in assignment")
                            .with_help(format!("cannot assign '{}' to '{}'",
                                self.type_to_string(&value_type),
                                self.type_to_string(&target_type))),
                        span.offset,
                        span.length,
                        "assignment here"
                    );
                } else if self.is_narrowing_conversion(&target_type, &value_type) {
                    self.reporter.report_with_label(
                        Diagnostic::warning(codes::NARROWING_CONVERSION,
                            "narrowing conversion in assignment")
                            .with_help(format!("converting from '{}' to '{}' may lose data",
                                self.type_to_string(&value_type),
                                self.type_to_string(&target_type))),
                        span.offset,
                        span.length,
                        "narrowing here"
                    );
                }

                Some(target_type)
            }

            Expr::Call { callee, args, span } => {
                // Get function type
                if let Expr::Identifier(name) = callee.as_ref() {
                    // Look up function signature
                    let func_def = self.symbols.lookup_function(name).cloned();
                    
                    if let Some(func) = func_def {
                        self.symbols.mark_used(name);
                        
                        // Validate argument count
                        let expected = func.params.len();
                        let got = args.len();
                        
                        if expected != got {
                            self.reporter.report_with_label(
                                Diagnostic::error(codes::TYPE_MISMATCH,
                                    format!("function '{}' expects {} argument{}, got {}",
                                        name, expected, if expected == 1 { "" } else { "s" }, got)),
                                span.offset,
                                span.length,
                                "called here"
                            );
                        }
                        
                        // Check each argument type
                        for (i, arg) in args.iter().enumerate() {
                            if let Some(arg_type) = self.check_expr(arg) {
                                if i < func.params.len() {
                                    let param = &func.params[i];
                                    if !self.types_compatible(&param.type_spec, &arg_type) {
                                        let fallback = format!("argument {}", i + 1);
                                        let param_name = param.name.as_deref().unwrap_or(&fallback);
                                        self.reporter.report_with_label(
                                            Diagnostic::error(codes::TYPE_MISMATCH,
                                                format!("type mismatch for parameter '{}'", param_name))
                                                .with_help(format!("expected '{}', found '{}'",
                                                    self.type_to_string(&param.type_spec),
                                                    self.type_to_string(&arg_type))),
                                            span.offset,
                                            span.length,
                                            &format!("argument {} here", i + 1)
                                        );
                                    }
                                }
                            }
                        }
                        
                        return Some(func.return_type.clone());
                    } else {
                        // Could be a C library function - allow it but still check args
                        for arg in args {
                            self.check_expr(arg);
                        }
                        return Some(TypeSpec {
                            base: BaseType::Int,
                            pointer_depth: 0,
                            is_const: false,
                            is_volatile: false,
                        });
                    }
                }
                
                for arg in args {
                    self.check_expr(arg);
                }
                
                None
            }

            Expr::Index { array, index } => {
                let array_type = self.check_expr(array)?;
                let index_type = self.check_expr(index)?;

                // Index must be integer type
                if !self.is_integer_type(&index_type) {
                    self.reporter.add(
                        Diagnostic::error(codes::TYPE_MISMATCH,
                            "array index must be an integer type")
                    );
                }

                // Array must be pointer or array type
                if array_type.pointer_depth == 0 {
                    self.reporter.add(
                        Diagnostic::error(codes::INDEX_NON_ARRAY,
                            "cannot index non-array type")
                    );
                    return None;
                }

                // Result type is element type (reduce pointer depth by 1)
                Some(TypeSpec {
                    base: array_type.base,
                    pointer_depth: array_type.pointer_depth - 1,
                    is_const: array_type.is_const,
                    is_volatile: array_type.is_volatile,
                })
            }

            Expr::Member { object, member } => {
                let obj_type = self.check_expr(object)?;
                
                // Get struct name from the object type
                let struct_name = match &obj_type.base {
                    BaseType::Struct(name) => name.clone(),
                    _ => {
                        self.reporter.add(
                            Diagnostic::error(codes::TYPE_MISMATCH,
                                format!("'.' operator requires a struct type, found '{}'",
                                    self.type_to_string(&obj_type)))
                        );
                        return None;
                    }
                };
                
                // Look up the struct definition
                if let Some(TypeDef::Struct(struct_def)) = self.symbols.lookup_type(&struct_name) {
                    // Find the field by name
                    if let Some(field) = struct_def.fields.iter().find(|f| &f.name == member) {
                        Some(field.type_spec.clone())
                    } else {
                        self.reporter.add(
                            Diagnostic::error(codes::UNDEFINED_VARIABLE,
                                format!("struct '{}' has no field named '{}'", struct_name, member))
                        );
                        None
                    }
                } else {
                    self.reporter.add(
                        Diagnostic::error(codes::UNDEFINED_VARIABLE,
                            format!("unknown struct type '{}'", struct_name))
                    );
                    None
                }
            }

            Expr::PtrMember { pointer, member } => {
                let ptr_type = self.check_expr(pointer)?;
                
                if ptr_type.pointer_depth == 0 {
                    self.reporter.add(
                        Diagnostic::error(codes::DEREFERENCE_NON_POINTER,
                            "'->' requires a pointer type")
                    );
                    return None;
                }
                
                // Get struct name from the pointer type
                let struct_name = match &ptr_type.base {
                    BaseType::Struct(name) => name.clone(),
                    _ => {
                        self.reporter.add(
                            Diagnostic::error(codes::TYPE_MISMATCH,
                                format!("'->' operator requires a pointer to struct type, found '{}'",
                                    self.type_to_string(&ptr_type)))
                        );
                        return None;
                    }
                };
                
                // Look up the struct definition
                if let Some(TypeDef::Struct(struct_def)) = self.symbols.lookup_type(&struct_name) {
                    // Find the field by name
                    if let Some(field) = struct_def.fields.iter().find(|f| &f.name == member) {
                        Some(field.type_spec.clone())
                    } else {
                        self.reporter.add(
                            Diagnostic::error(codes::UNDEFINED_VARIABLE,
                                format!("struct '{}' has no field named '{}'", struct_name, member))
                        );
                        None
                    }
                } else {
                    self.reporter.add(
                        Diagnostic::error(codes::UNDEFINED_VARIABLE,
                            format!("unknown struct type '{}'", struct_name))
                    );
                    None
                }
            }

            Expr::Cast { type_spec, expr } => {
                self.check_expr(expr);
                Some(type_spec.clone())
            }

            Expr::Sizeof(_) => Some(TypeSpec {
                base: BaseType::Uint64, // size_t
                pointer_depth: 0,
                is_const: true,
                is_volatile: false,
            }),

            Expr::Ternary { condition, then_expr, else_expr } => {
                if let Some(cond_type) = self.check_expr(condition) {
                    self.check_condition_type(&cond_type);
                }
                
                let then_type = self.check_expr(then_expr);
                let else_type = self.check_expr(else_expr);
                
                // Types should match
                if let (Some(t), Some(e)) = (&then_type, &else_type) {
                    if !self.types_compatible(t, e) {
                        self.reporter.add(
                            Diagnostic::error(codes::TYPE_MISMATCH,
                                "ternary branches have incompatible types")
                        );
                    }
                }
                
                then_type
            }

            Expr::Comma(exprs) => {
                let mut last_type = None;
                for e in exprs {
                    last_type = self.check_expr(e);
                }
                last_type
            }

            Expr::InitList(exprs) => {
                for e in exprs {
                    self.check_expr(e);
                }
                None // Type depends on context
            }

            Expr::DesignatedInit(fields) => {
                for (_, e) in fields {
                    self.check_expr(e);
                }
                None
            }

            Expr::Unwrap(expr) => {
                let expr_type = self.check_expr(expr)?;
                
                if !expr_type.is_option() {
                    self.reporter.add(
                        Diagnostic::error(codes::TYPE_MISMATCH,
                            ".unwrap() can only be used on Option types")
                    );
                }
                
                // Return inner type
                if let BaseType::Option(inner) = &expr_type.base {
                    Some(inner.as_ref().clone())
                } else {
                    None
                }
            }

            Expr::UnwrapOr { expr, default } => {
                let expr_type = self.check_expr(expr)?;
                self.check_expr(default);
                
                if !expr_type.is_option() {
                    self.reporter.add(
                        Diagnostic::error(codes::TYPE_MISMATCH,
                            ".unwrap_or() can only be used on Option types")
                    );
                }
                
                if let BaseType::Option(inner) = &expr_type.base {
                    Some(inner.as_ref().clone())
                } else {
                    None
                }
            }
            
            Expr::IsSome(expr) => {
                let expr_type = self.check_expr(expr)?;
                
                if !expr_type.is_option() {
                    self.reporter.add(
                        Diagnostic::error(codes::TYPE_MISMATCH,
                            ".is_some() can only be used on Option types")
                    );
                }
                
                // Returns bool
                Some(TypeSpec {
                    base: BaseType::Bool,
                    pointer_depth: 0,
                    is_const: false,
                    is_volatile: false,
                })
            }
            
            Expr::IsNone(expr) => {
                let expr_type = self.check_expr(expr)?;
                
                if !expr_type.is_option() {
                    self.reporter.add(
                        Diagnostic::error(codes::TYPE_MISMATCH,
                            ".is_none() can only be used on Option types")
                    );
                }
                
                // Returns bool
                Some(TypeSpec {
                    base: BaseType::Bool,
                    pointer_depth: 0,
                    is_const: false,
                    is_volatile: false,
                })
            }
        }
    }

    /// Check if a condition type is valid (must be scalar)
    fn check_condition_type(&mut self, type_spec: &TypeSpec) {
        // Pointers and integers are valid conditions
        // Bool is explicitly valid
        // Floats are valid (C allows it)
        // Structs are not valid
        
        if matches!(type_spec.base, BaseType::Struct(_) | BaseType::Union(_)) 
            && type_spec.pointer_depth == 0 
        {
            self.reporter.add(
                Diagnostic::error(codes::TYPE_MISMATCH,
                    "condition must be a scalar type")
            );
        }
    }

    /// Check binary operation types
    fn check_binary_types(&mut self, left: &TypeSpec, op: BinaryOp, right: &TypeSpec, span: Span) -> Option<TypeSpec> {
        // For now, just check basic compatibility
        // A full implementation would handle numeric promotions
        
        match op {
            BinaryOp::And | BinaryOp::Or => {
                // Logical operators return bool
                Some(TypeSpec {
                    base: BaseType::Bool,
                    pointer_depth: 0,
                    is_const: false,
                    is_volatile: false,
                })
            }
            
            BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | 
            BinaryOp::Gt | BinaryOp::Ge => {
                // Check for signed/unsigned comparison
                if self.is_signed_unsigned_comparison(left, right) {
                    self.reporter.report_with_label(
                        Diagnostic::warning(codes::SIGNED_UNSIGNED_COMPARE,
                            "comparison between signed and unsigned integers")
                            .with_help(format!("comparing '{}' with '{}'",
                                self.type_to_string(left),
                                self.type_to_string(right))),
                        span.offset,
                        span.length,
                        "comparison here"
                    );
                }
                
                // Comparison operators return bool
                Some(TypeSpec {
                    base: BaseType::Bool,
                    pointer_depth: 0,
                    is_const: false,
                    is_volatile: false,
                })
            }
            
            _ => {
                // Arithmetic operators - use left type for now
                Some(left.clone())
            }
        }
    }

    /// Check unary operation type
    fn check_unary_type(&mut self, op: UnaryOp, operand: &TypeSpec) -> Option<TypeSpec> {
        match op {
            UnaryOp::AddrOf => {
                // Address-of increases pointer depth
                Some(TypeSpec {
                    base: operand.base.clone(),
                    pointer_depth: operand.pointer_depth + 1,
                    is_const: false,
                    is_volatile: operand.is_volatile,
                })
            }
            
            UnaryOp::Deref => {
                if operand.pointer_depth == 0 {
                    self.reporter.add(
                        Diagnostic::error(codes::DEREFERENCE_NON_POINTER,
                            "cannot dereference non-pointer type")
                    );
                    return None;
                }
                
                Some(TypeSpec {
                    base: operand.base.clone(),
                    pointer_depth: operand.pointer_depth - 1,
                    is_const: operand.is_const,
                    is_volatile: operand.is_volatile,
                })
            }
            
            UnaryOp::Not => {
                Some(TypeSpec {
                    base: BaseType::Bool,
                    pointer_depth: 0,
                    is_const: false,
                    is_volatile: false,
                })
            }
            
            _ => Some(operand.clone()),
        }
    }

    /// Check if two types are compatible
    fn types_compatible(&self, a: &TypeSpec, b: &TypeSpec) -> bool {
        // Same pointer depth
        if a.pointer_depth != b.pointer_depth {
            return false;
        }
        
        // Same base type (simplified - a full impl would handle promotions)
        self.base_types_compatible(&a.base, &b.base)
    }

    /// Check if two base types are compatible
    fn base_types_compatible(&self, a: &BaseType, b: &BaseType) -> bool {
        use BaseType::*;
        
        // Check if both are integer types - all integers are compatible (with narrowing warnings)
        let is_int_a = matches!(a, Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16 | Uint32 | Uint64 |
            Int | Long | Short | UnsignedInt | UnsignedLong | UnsignedShort | Char | UnsignedChar | Bool);
        let is_int_b = matches!(b, Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16 | Uint32 | Uint64 |
            Int | Long | Short | UnsignedInt | UnsignedLong | UnsignedShort | Char | UnsignedChar | Bool);
        
        if is_int_a && is_int_b {
            return true;
        }
        
        // Check if both are floating point types
        let is_float_a = matches!(a, Float | Float32 | Double | Float64);
        let is_float_b = matches!(b, Float | Float32 | Double | Float64);
        
        if is_float_a && is_float_b {
            return true;
        }
        
        // Allow int <-> float conversions (C allows this)
        if (is_int_a && is_float_b) || (is_float_a && is_int_b) {
            return true;
        }
        
        match (a, b) {
            // Void
            (Void, Void) => true,
            
            // Named types
            (Named(a), Named(b)) => a == b,
            (Struct(a), Struct(b)) => a == b,
            (Enum(a), Enum(b)) => a == b,
            (Union(a), Union(b)) => a == b,
            
            // Enums are compatible with int
            (Enum(_), _) if is_int_b => true,
            (_, Enum(_)) if is_int_a => true,
            
            // Option types - compare inner types
            (Option(inner_a), Option(inner_b)) => {
                self.types_compatible(inner_a, inner_b)
            }
            
            // Void is compatible with any Option (for 'none' literal)
            (Void, Option(_)) | (Option(_), Void) => true,
            
            _ => false,
        }
    }

    /// Check if type is an integer type
    fn is_integer_type(&self, type_spec: &TypeSpec) -> bool {
        if type_spec.pointer_depth > 0 {
            return false;
        }
        
        matches!(type_spec.base,
            BaseType::Int8 | BaseType::Int16 | BaseType::Int32 | BaseType::Int64 |
            BaseType::Uint8 | BaseType::Uint16 | BaseType::Uint32 | BaseType::Uint64 |
            BaseType::Int | BaseType::Long | BaseType::Short |
            BaseType::UnsignedInt | BaseType::UnsignedLong | BaseType::UnsignedShort |
            BaseType::Char | BaseType::UnsignedChar | BaseType::Bool
        )
    }
    
    /// Get the size in bits of a numeric type (for narrowing detection)
    fn numeric_type_size(&self, base: &BaseType) -> Option<u8> {
        match base {
            BaseType::Bool => Some(1),
            BaseType::Int8  => Some(8),
            BaseType::Uint8 => Some(8),
            BaseType::Char  => Some(8),
            BaseType::UnsignedChar => Some(8),
            BaseType::Int16 | BaseType::Short => Some(16),
            BaseType::Uint16 | BaseType::UnsignedShort => Some(16),
            BaseType::Int32 | BaseType::Int => Some(32),
            BaseType::Uint32 | BaseType::UnsignedInt => Some(32),
            BaseType::Float | BaseType::Float32 => Some(32),
            BaseType::Int64 | BaseType::Long => Some(64),
            BaseType::Uint64 | BaseType::UnsignedLong => Some(64),
            BaseType::Double | BaseType::Float64 => Some(64),
            _ => None,
        }
    }
    
    /// Check if assigning from source to target is a narrowing conversion
    fn is_narrowing_conversion(&self, target: &TypeSpec, source: &TypeSpec) -> bool {
        // Only check non-pointer numeric types
        if target.pointer_depth > 0 || source.pointer_depth > 0 {
            return false;
        }
        
        if let (Some(target_size), Some(source_size)) = (
            self.numeric_type_size(&target.base),
            self.numeric_type_size(&source.base)
        ) {
            source_size > target_size
        } else {
            false
        }
    }
    
    /// Check if a type is signed
    fn is_signed_type(&self, base: &BaseType) -> Option<bool> {
        match base {
            BaseType::Int8 | BaseType::Int16 | BaseType::Int32 | BaseType::Int64 |
            BaseType::Int | BaseType::Long | BaseType::Short | BaseType::Char => Some(true),
            BaseType::Uint8 | BaseType::Uint16 | BaseType::Uint32 | BaseType::Uint64 |
            BaseType::UnsignedInt | BaseType::UnsignedLong | BaseType::UnsignedShort |
            BaseType::UnsignedChar => Some(false),
            _ => None,
        }
    }
    
    /// Check if comparing signed and unsigned types
    fn is_signed_unsigned_comparison(&self, left: &TypeSpec, right: &TypeSpec) -> bool {
        if left.pointer_depth > 0 || right.pointer_depth > 0 {
            return false;
        }
        
        if let (Some(left_signed), Some(right_signed)) = (
            self.is_signed_type(&left.base),
            self.is_signed_type(&right.base)
        ) {
            left_signed != right_signed
        } else {
            false
        }
    }

    /// Convert type to string for error messages
    fn type_to_string(&self, type_spec: &TypeSpec) -> String {
        let mut s = type_spec.base.to_c_type();
        for _ in 0..type_spec.pointer_depth {
            s.push('*');
        }
        s
    }
}
