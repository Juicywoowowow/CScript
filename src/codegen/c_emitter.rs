//! C code emitter for transpiling CScript AST to C.

use crate::parser::*;
use std::collections::HashSet;

/// Emits C code from CScript AST
pub struct CEmitter {
    output: String,
    indent: usize,
    /// Track Option types used (to generate their struct definitions)
    option_types: HashSet<String>,
    /// Track Result types used (to generate their struct definitions)
    result_types: HashSet<String>,
    /// Whether to emit bounds checks
    bounds_checks: bool,
    /// Current function's return type (for Option/Result implicit wrapping)
    current_return_type: Option<TypeSpec>,
    /// Counter for generating unique temporary variable names
    temp_counter: usize,
}

impl CEmitter {
    pub fn new() -> Self {
        Self {
            output: String::new(),
            indent: 0,
            option_types: HashSet::new(),
            result_types: HashSet::new(),
            bounds_checks: false,
            current_return_type: None,
            temp_counter: 0,
        }
    }

    pub fn set_bounds_checks(&mut self, enabled: bool) {
        self.bounds_checks = enabled;
    }

    /// Emit C code from a program
    pub fn emit(&mut self, program: &Program) -> String {
        // First pass: collect all Option and Result types used
        self.collect_option_types(program);

        // Emit header
        self.emit_header();

        // Emit Option type definitions
        self.emit_option_types();
        
        // Emit Result type definitions
        self.emit_result_types();

        // Emit declarations
        for decl in &program.declarations {
            self.emit_declaration(decl);
            self.newline();
        }

        std::mem::take(&mut self.output)
    }

    /// Collect all Option<T> types used in the program
    fn collect_option_types(&mut self, program: &Program) {
        for decl in &program.declarations {
            self.scan_declaration_for_options(decl);
        }
    }

    fn scan_declaration_for_options(&mut self, decl: &Declaration) {
        match decl {
            Declaration::Function(f) => {
                self.scan_type_for_option(&f.return_type);
                for param in &f.params {
                    self.scan_type_for_option(&param.type_spec);
                }
                if let Some(ref body) = f.body {
                    self.scan_block_for_options(body);
                }
            }
            Declaration::Variable(v) => {
                self.scan_type_for_option(&v.type_spec);
            }
            Declaration::Struct(s) => {
                for field in &s.fields {
                    self.scan_type_for_option(&field.type_spec);
                }
            }
            _ => {}
        }
    }

    fn scan_block_for_options(&mut self, block: &Block) {
        for stmt in &block.statements {
            self.scan_stmt_for_options(stmt);
        }
    }

    fn scan_stmt_for_options(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VarDecl(v) => self.scan_type_for_option(&v.type_spec),
            Stmt::Block(b) => self.scan_block_for_options(b),
            Stmt::If { then_branch, else_branch, .. } => {
                self.scan_stmt_for_options(then_branch);
                if let Some(ref e) = else_branch {
                    self.scan_stmt_for_options(e);
                }
            }
            Stmt::While { body, .. } => self.scan_stmt_for_options(body),
            Stmt::DoWhile { body, .. } => self.scan_stmt_for_options(body),
            Stmt::For { init, body, .. } => {
                if let Some(ref i) = init {
                    self.scan_stmt_for_options(i);
                }
                self.scan_stmt_for_options(body);
            }
            Stmt::Match { arms, .. } => {
                for arm in arms {
                    self.scan_block_for_options(&arm.body);
                }
            }
            _ => {}
        }
    }

    fn scan_type_for_option(&mut self, type_spec: &TypeSpec) {
        if let BaseType::Option(inner) = &type_spec.base {
            let type_name = self.option_type_name(inner);
            self.option_types.insert(type_name);
            // Recursively scan inner type
            self.scan_type_for_option(inner);
        }
        if let BaseType::Result(ok_type, err_type) = &type_spec.base {
            let type_name = self.result_type_name(ok_type, err_type);
            self.result_types.insert(type_name);
            // Recursively scan inner types
            self.scan_type_for_option(ok_type);
            self.scan_type_for_option(err_type);
        }
    }

    fn option_type_name(&self, inner: &TypeSpec) -> String {
        let mut name = format!("Option_{}", inner.base.to_c_type().replace(' ', "_"));
        for _ in 0..inner.pointer_depth {
            name.push_str("_ptr");
        }
        name
    }
    
    fn result_type_name(&self, ok_type: &TypeSpec, err_type: &TypeSpec) -> String {
        let ok_name = self.type_to_c_name(ok_type);
        let err_name = self.type_to_c_name(err_type);
        format!("Result_{}_{}", ok_name, err_name)
    }
    
    fn type_to_c_name(&self, type_spec: &TypeSpec) -> String {
        let mut name = type_spec.base.to_c_type().replace(' ', "_").replace('*', "");
        for _ in 0..type_spec.pointer_depth {
            name.push_str("_ptr");
        }
        name
    }
    
    /// Get Option type name from a TypeSpec that is known to be Option<T>
    fn option_type_name_from_spec(&self, spec: &TypeSpec) -> String {
        if let BaseType::Option(inner) = &spec.base {
            self.option_type_name(inner)
        } else {
            // Fallback - shouldn't happen
            "Option_unknown".to_string()
        }
    }
    
    /// Get Result type name from a TypeSpec that is known to be Result<T, E>
    fn result_type_name_from_spec(&self, spec: &TypeSpec) -> String {
        if let BaseType::Result(ok_type, err_type) = &spec.base {
            self.result_type_name(ok_type, err_type)
        } else {
            "Result_unknown".to_string()
        }
    }

    /// Emit the C file header
    fn emit_header(&mut self) {
        self.writeln("/* Generated by CScript Compiler */");
        self.writeln("/* Do not edit - regenerate from .csr source */");
        self.newline();
        self.writeln("#include <stdint.h>");
        self.writeln("#include <stdbool.h>");
        self.writeln("#include <stddef.h>");
        
        if self.bounds_checks {
            self.writeln("#include <stdlib.h>");
            self.writeln("#include <stdio.h>");
            self.newline();
            self.writeln("#define CSC_BOUNDS_CHECK(idx, len) \\");
            self.writeln("    do { if ((size_t)(idx) >= (size_t)(len)) { \\");
            self.writeln("        fprintf(stderr, \"CScript: array index out of bounds at %s:%d\\n\", __FILE__, __LINE__); \\");
            self.writeln("        abort(); \\");
            self.writeln("    }} while(0)");
        }
        
        self.newline();
    }

    /// Emit Option type struct definitions
    fn emit_option_types(&mut self) {
        if self.option_types.is_empty() {
            return;
        }

        self.writeln("/* Option type definitions */");
        
        for type_name in &self.option_types.clone() {
            self.writeln(&format!("typedef struct {{"));
            self.indent += 1;
            self.write_indent();
            self.writeln("bool is_some;");
            self.write_indent();
            self.writeln("void* ok_value;");  // Use ok_value for consistency with Result
            self.indent -= 1;
            self.writeln(&format!("}} {};", type_name));
            self.newline();

            // Helper macros for this Option type
            self.writeln(&format!("#define {}_SOME(v) (({}){{ .is_some = true, .ok_value = (void*)(v) }})", 
                type_name, type_name));
            self.writeln(&format!("#define {}_NONE (({}){{ .is_some = false, .ok_value = NULL }})", 
                type_name, type_name));
            self.newline();
        }
    }
    
    /// Emit Result type struct definitions
    fn emit_result_types(&mut self) {
        if self.result_types.is_empty() {
            return;
        }

        self.writeln("/* Result type definitions */");
        
        for type_name in &self.result_types.clone() {
            self.writeln(&format!("typedef struct {{"));
            self.indent += 1;
            self.write_indent();
            self.writeln("bool is_ok;");
            self.write_indent();
            self.writeln("union {");
            self.indent += 1;
            self.write_indent();
            self.writeln("void* ok_value;");
            self.write_indent();
            self.writeln("void* err_value;");
            self.indent -= 1;
            self.write_indent();
            self.writeln("};");
            self.indent -= 1;
            self.writeln(&format!("}} {};", type_name));
            self.newline();

            // Helper macros for this Result type
            self.writeln(&format!("#define {}_OK(v) (({}){{ .is_ok = true, .ok_value = (void*)(intptr_t)(v) }})", 
                type_name, type_name));
            self.writeln(&format!("#define {}_ERR(e) (({}){{ .is_ok = false, .err_value = (void*)(intptr_t)(e) }})", 
                type_name, type_name));
            self.newline();
        }
    }

    /// Emit a declaration
    fn emit_declaration(&mut self, decl: &Declaration) {
        match decl {
            Declaration::Function(f) => self.emit_function(f),
            Declaration::Variable(v) => self.emit_global_variable(v),
            Declaration::Struct(s) => self.emit_struct(s),
            Declaration::Enum(e) => self.emit_enum(e),
            Declaration::Union(u) => self.emit_union(u),
            Declaration::Typedef(t) => self.emit_typedef(t),
            Declaration::Preprocessor(p) => self.writeln(p),
        }
    }

    /// Emit a function
    fn emit_function(&mut self, func: &FunctionDecl) {
        // Modifiers
        if func.is_static {
            self.write("static ");
        }
        if func.is_inline {
            self.write("inline ");
        }
        if func.is_extern {
            self.write("extern ");
        }

        // Return type
        self.emit_type(&func.return_type);
        self.write(" ");

        // Name and parameters
        self.write(&func.name);
        self.write("(");

        for (i, param) in func.params.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            self.emit_type(&param.type_spec);
            if let Some(ref name) = param.name {
                self.write(" ");
                self.write(name);
            }
        }

        if func.params.is_empty() {
            self.write("void");
        }

        self.write(")");

        // Body or declaration
        if let Some(ref body) = func.body {
            // Track return type for Option implicit wrapping
            self.current_return_type = Some(func.return_type.clone());
            self.write(" ");
            self.emit_block(body);
            self.current_return_type = None;
        } else {
            self.writeln(";");
        }
    }

    /// Emit a global variable
    fn emit_global_variable(&mut self, var: &VariableDecl) {
        self.write_indent();

        if var.is_static {
            self.write("static ");
        }
        if var.is_extern {
            self.write("extern ");
        }

        // In C, immutable globals use const
        if !var.is_mutable {
            self.write("const ");
        }

        self.emit_type(&var.type_spec);
        self.write(" ");
        self.write(&var.name);

        // Array dimensions
        for dim in &var.array_dims {
            self.write("[");
            if let Some(ref expr) = dim {
                self.emit_expr(expr);
            }
            self.write("]");
        }

        // Initializer
        if let Some(ref init) = var.initializer {
            self.write(" = ");
            self.emit_expr(init);
        }

        self.writeln(";");
    }

    /// Emit a struct
    fn emit_struct(&mut self, s: &StructDecl) {
        self.write("struct ");
        if let Some(ref name) = s.name {
            self.write(name);
            self.write(" ");
        }

        if !s.fields.is_empty() {
            self.writeln("{");
            self.indent += 1;

            for field in &s.fields {
                self.write_indent();
                self.emit_type(&field.type_spec);
                self.write(" ");
                self.write(&field.name);

                for dim in &field.array_dims {
                    self.write("[");
                    if let Some(ref expr) = dim {
                        self.emit_expr(expr);
                    }
                    self.write("]");
                }

                self.writeln(";");
            }

            self.indent -= 1;
            self.write_indent();
            self.writeln("};");
        } else {
            self.writeln(";");
        }
    }

    /// Emit an enum
    fn emit_enum(&mut self, e: &EnumDecl) {
        self.write("enum ");
        if let Some(ref name) = e.name {
            self.write(name);
            self.write(" ");
        }

        if !e.variants.is_empty() {
            self.writeln("{");
            self.indent += 1;

            for (i, variant) in e.variants.iter().enumerate() {
                self.write_indent();
                self.write(&variant.name);

                if let Some(ref value) = variant.value {
                    self.write(" = ");
                    self.emit_expr(value);
                }

                if i < e.variants.len() - 1 {
                    self.writeln(",");
                } else {
                    self.newline();
                }
            }

            self.indent -= 1;
            self.write_indent();
            self.writeln("};");
        } else {
            self.writeln(";");
        }
    }

    /// Emit a union
    fn emit_union(&mut self, u: &UnionDecl) {
        self.write("union ");
        if let Some(ref name) = u.name {
            self.write(name);
            self.write(" ");
        }

        if !u.fields.is_empty() {
            self.writeln("{");
            self.indent += 1;

            for field in &u.fields {
                self.write_indent();
                self.emit_type(&field.type_spec);
                self.write(" ");
                self.write(&field.name);
                self.writeln(";");
            }

            self.indent -= 1;
            self.write_indent();
            self.writeln("};");
        } else {
            self.writeln(";");
        }
    }

    /// Emit a typedef
    fn emit_typedef(&mut self, t: &TypedefDecl) {
        self.write("typedef ");
        self.emit_type(&t.original_type);
        self.write(" ");
        self.write(&t.new_name);
        self.writeln(";");
    }

    /// Emit a type
    fn emit_type(&mut self, type_spec: &TypeSpec) {
        if type_spec.is_volatile {
            self.write("volatile ");
        }

        match &type_spec.base {
            BaseType::Option(inner) => {
                let name = self.option_type_name(inner);
                self.write(&name);
            }
            BaseType::Result(ok_type, err_type) => {
                let name = self.result_type_name(ok_type, err_type);
                self.write(&name);
            }
            _ => {
                self.write(&type_spec.base.to_c_type());
            }
        }

        for _ in 0..type_spec.pointer_depth {
            self.write("*");
        }
    }

    /// Emit a block
    fn emit_block(&mut self, block: &Block) {
        self.writeln("{");
        self.indent += 1;

        for stmt in &block.statements {
            self.emit_stmt(stmt);
        }

        self.indent -= 1;
        self.write_indent();
        self.writeln("}");
    }
    
    /// Emit a variable declaration with a Try (?) expression
    /// Expands: `int x = foo()?;` into:
    /// ```c
    /// Result_T_E _tmp = foo();
    /// if (!_tmp.is_ok) { return Result_T_E_ERR(_tmp.err_value); }
    /// int x = _tmp.ok_value;
    /// ```
    fn emit_try_var_decl(&mut self, var: &VariableDecl, try_expr: &Expr) {
        let temp_name = format!("_try_{}", self.temp_counter);
        self.temp_counter += 1;
        
        // Get the Result type from the current function's return type
        let result_type_name = if let Some(ref ret_type) = self.current_return_type {
            self.result_type_name_from_spec(ret_type)
        } else {
            "Result_unknown".to_string()
        };
        
        // Emit: Result_T_E _tmp = expr;
        self.write_indent();
        self.write(&result_type_name);
        self.write(" ");
        self.write(&temp_name);
        self.write(" = ");
        self.emit_expr(try_expr);
        self.writeln(";");
        
        // Emit: if (!_tmp.is_ok) { return Result_T_E_ERR(_tmp.err_value); }
        self.write_indent();
        self.write(&format!("if (!{}.is_ok) {{ return {}_ERR({}.err_value); }}", 
            temp_name, result_type_name, temp_name));
        self.newline();
        
        // Emit: T var_name = _tmp.ok_value;
        self.write_indent();
        if !var.is_mutable {
            self.write("const ");
        }
        self.emit_type(&var.type_spec);
        self.write(" ");
        self.write(&var.name);
        self.write(&format!(" = {}.ok_value;", temp_name));
        self.newline();
    }

    /// Emit a statement
    fn emit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expression(expr) => {
                self.write_indent();
                self.emit_expr(expr);
                self.writeln(";");
            }

            Stmt::VarDecl(var) => {
                // Check if initializer contains a Try (?) expression
                if let Some(ref init) = var.initializer {
                    if let Expr::Try { expr, .. } = init {
                        // Expand the ? operator
                        self.emit_try_var_decl(var, expr);
                        return;
                    }
                }
                
                self.write_indent();

                if var.is_static {
                    self.write("static ");
                }

                if !var.is_mutable {
                    self.write("const ");
                }

                self.emit_type(&var.type_spec);
                self.write(" ");
                self.write(&var.name);

                for dim in &var.array_dims {
                    self.write("[");
                    if let Some(ref expr) = dim {
                        self.emit_expr(expr);
                    }
                    self.write("]");
                }

                if let Some(ref init) = var.initializer {
                    self.write(" = ");
                    self.emit_expr(init);
                }

                self.writeln(";");
            }

            Stmt::Block(block) => {
                self.write_indent();
                self.emit_block(block);
            }

            Stmt::If { condition, then_branch, else_branch } => {
                self.write_indent();
                self.write("if (");
                self.emit_expr(condition);
                self.write(") ");
                
                if matches!(then_branch.as_ref(), Stmt::Block(_)) {
                    self.emit_stmt_inline(then_branch);
                } else {
                    self.newline();
                    self.indent += 1;
                    self.emit_stmt(then_branch);
                    self.indent -= 1;
                }

                if let Some(ref else_stmt) = else_branch {
                    self.write_indent();
                    self.write("else ");
                    if matches!(else_stmt.as_ref(), Stmt::Block(_) | Stmt::If { .. }) {
                        self.emit_stmt_inline(else_stmt);
                    } else {
                        self.newline();
                        self.indent += 1;
                        self.emit_stmt(else_stmt);
                        self.indent -= 1;
                    }
                }
            }

            Stmt::While { condition, body } => {
                self.write_indent();
                self.write("while (");
                self.emit_expr(condition);
                self.write(") ");
                self.emit_stmt_inline(body);
            }

            Stmt::DoWhile { body, condition } => {
                self.write_indent();
                self.write("do ");
                self.emit_stmt_inline(body);
                self.write_indent();
                self.write("while (");
                self.emit_expr(condition);
                self.writeln(");");
            }

            Stmt::For { init, condition, update, body } => {
                self.write_indent();
                self.write("for (");

                if let Some(ref init_stmt) = init {
                    match init_stmt.as_ref() {
                        Stmt::VarDecl(var) => {
                            self.emit_type(&var.type_spec);
                            self.write(" ");
                            self.write(&var.name);
                            if let Some(ref init) = var.initializer {
                                self.write(" = ");
                                self.emit_expr(init);
                            }
                        }
                        Stmt::Expression(expr) => {
                            self.emit_expr(expr);
                        }
                        _ => {}
                    }
                }

                self.write("; ");

                if let Some(ref cond) = condition {
                    self.emit_expr(cond);
                }

                self.write("; ");

                if let Some(ref upd) = update {
                    self.emit_expr(upd);
                }

                self.write(") ");
                self.emit_stmt_inline(body);
            }

            Stmt::Switch { expr, cases, default } => {
                self.write_indent();
                self.write("switch (");
                self.emit_expr(expr);
                self.writeln(") {");

                for case in cases {
                    self.write_indent();
                    self.write("case ");
                    self.emit_expr(&case.value);
                    self.writeln(":");
                    
                    self.indent += 1;
                    for stmt in &case.body {
                        self.emit_stmt(stmt);
                    }
                    self.indent -= 1;
                }

                if let Some(ref default_block) = default {
                    self.write_indent();
                    self.writeln("default:");
                    self.indent += 1;
                    for stmt in &default_block.statements {
                        self.emit_stmt(stmt);
                    }
                    self.indent -= 1;
                }

                self.write_indent();
                self.writeln("}");
            }

            Stmt::Match { expr, arms } => {
                // Emit match as if-else chain using Option/Result struct fields
                self.write_indent();
                self.writeln("{");
                self.indent += 1;

                // Store the Option/Result value in a temp variable
                self.write_indent();
                self.write("const typeof(");
                self.emit_expr(expr);
                self.write(") _match_val = ");
                self.emit_expr(expr);
                self.writeln(";");

                for (i, arm) in arms.iter().enumerate() {
                    self.write_indent();
                    if i > 0 {
                        self.write("else ");
                    }

                    match &arm.pattern {
                        MatchPattern::Some(name) => {
                            self.writeln("if (_match_val.is_some) {");
                            self.indent += 1;
                            self.write_indent();
                            self.write("const typeof(_match_val.ok_value) ");
                            self.write(name);
                            self.writeln(" = _match_val.ok_value;");
                            
                            for stmt in &arm.body.statements {
                                self.emit_stmt(stmt);
                            }
                            
                            self.indent -= 1;
                            self.write_indent();
                            self.writeln("}");
                        }
                        MatchPattern::None => {
                            self.writeln("{");
                            self.indent += 1;
                            
                            for stmt in &arm.body.statements {
                                self.emit_stmt(stmt);
                            }
                            
                            self.indent -= 1;
                            self.write_indent();
                            self.writeln("}");
                        }
                        MatchPattern::Ok(name) => {
                            self.writeln("if (_match_val.is_ok) {");
                            self.indent += 1;
                            self.write_indent();
                            self.write("const typeof(_match_val.ok_value) ");
                            self.write(name);
                            self.writeln(" = _match_val.ok_value;");
                            
                            for stmt in &arm.body.statements {
                                self.emit_stmt(stmt);
                            }
                            
                            self.indent -= 1;
                            self.write_indent();
                            self.writeln("}");
                        }
                        MatchPattern::Err(name) => {
                            self.writeln("if (!_match_val.is_ok) {");
                            self.indent += 1;
                            self.write_indent();
                            self.write("const typeof(_match_val.err_value) ");
                            self.write(name);
                            self.writeln(" = _match_val.err_value;");
                            
                            for stmt in &arm.body.statements {
                                self.emit_stmt(stmt);
                            }
                            
                            self.indent -= 1;
                            self.write_indent();
                            self.writeln("}");
                        }
                    }
                }

                self.indent -= 1;
                self.write_indent();
                self.writeln("}");
            }

            Stmt::Return(expr) => {
                self.write_indent();
                self.write("return");
                
                if let Some(ref e) = expr {
                    self.write(" ");
                    
                    // Check if we need to wrap the value for Option/Result return type
                    if let Some(ref ret_type) = self.current_return_type.clone() {
                        if ret_type.is_option() {
                            // Check if the expression is 'none'
                            if matches!(e, Expr::None) {
                                // Return NONE macro
                                let type_name = self.option_type_name_from_spec(ret_type);
                                self.write(&format!("{}_NONE", type_name));
                            } else {
                                // Wrap non-None values with SOME macro
                                let type_name = self.option_type_name_from_spec(ret_type);
                                self.write(&format!("{}_SOME(", type_name));
                                self.emit_expr(e);
                                self.write(")");
                            }
                        } else if ret_type.is_result() {
                            // Check if the expression is ok() or err()
                            match e {
                                Expr::Ok(val) => {
                                    let type_name = self.result_type_name_from_spec(ret_type);
                                    self.write(&format!("{}_OK(", type_name));
                                    self.emit_expr(val);
                                    self.write(")");
                                }
                                Expr::Err(val) => {
                                    let type_name = self.result_type_name_from_spec(ret_type);
                                    self.write(&format!("{}_ERR(", type_name));
                                    self.emit_expr(val);
                                    self.write(")");
                                }
                                _ => {
                                    // Assume it's already a Result or needs wrapping as Ok
                                    self.emit_expr(e);
                                }
                            }
                        } else {
                            self.emit_expr(e);
                        }
                    } else {
                        self.emit_expr(e);
                    }
                }
                self.writeln(";");
            }

            Stmt::Break => {
                self.write_indent();
                self.writeln("break;");
            }

            Stmt::Continue => {
                self.write_indent();
                self.writeln("continue;");
            }

            Stmt::Goto(label) => {
                self.write_indent();
                self.write("goto ");
                self.write(label);
                self.writeln(";");
            }

            Stmt::Label(name) => {
                // Labels are not indented
                self.write(name);
                self.writeln(":;");
            }

            Stmt::Empty => {
                self.write_indent();
                self.writeln(";");
            }
            
            Stmt::StructDecl(s) => {
                self.write_indent();
                self.emit_struct(s);
            }
        }
    }

    /// Emit a statement without leading indent (for inline after if/while/etc.)
    fn emit_stmt_inline(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block(block) => {
                self.emit_block(block);
            }
            _ => {
                self.newline();
                self.indent += 1;
                self.emit_stmt(stmt);
                self.indent -= 1;
            }
        }
    }

    /// Emit an expression
    fn emit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::IntLiteral(n) => {
                self.write(&n.to_string());
            }

            Expr::FloatLiteral(f) => {
                self.write(&format!("{:?}", f));
            }

            Expr::StringLiteral(s) => {
                self.write("\"");
                self.write(&escape_string(s));
                self.write("\"");
            }

            Expr::CharLiteral(c) => {
                self.write("'");
                self.write(&escape_char(*c));
                self.write("'");
            }

            Expr::BoolLiteral(b) => {
                self.write(if *b { "true" } else { "false" });
            }

            Expr::None => {
                self.write("NULL");
            }

            Expr::Identifier(name) => {
                self.write(name);
            }

            Expr::Binary { left, op, right, .. } => {
                self.write("(");
                self.emit_expr(left);
                self.write(" ");
                self.write(op.to_c_str());
                self.write(" ");
                self.emit_expr(right);
                self.write(")");
            }

            Expr::Unary { op, operand } => {
                match op {
                    UnaryOp::PostInc | UnaryOp::PostDec => {
                        self.write("(");
                        self.emit_expr(operand);
                        self.write(op.to_c_str());
                        self.write(")");
                    }
                    _ => {
                        self.write("(");
                        self.write(op.to_c_str());
                        self.emit_expr(operand);
                        self.write(")");
                    }
                }
            }

            Expr::Assign { target, op, value, .. } => {
                self.emit_expr(target);
                self.write(" ");
                self.write(op.to_c_str());
                self.write(" ");
                self.emit_expr(value);
            }

            Expr::Call { callee, args, .. } => {
                self.emit_expr(callee);
                self.write("(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.emit_expr(arg);
                }
                self.write(")");
            }

            Expr::Index { array, index } => {
                self.emit_expr(array);
                self.write("[");
                self.emit_expr(index);
                self.write("]");
            }

            Expr::Member { object, member } => {
                self.emit_expr(object);
                self.write(".");
                self.write(member);
            }

            Expr::PtrMember { pointer, member } => {
                self.emit_expr(pointer);
                self.write("->");
                self.write(member);
            }

            Expr::Cast { type_spec, expr } => {
                self.write("(");
                self.emit_type(type_spec);
                self.write(")");
                self.emit_expr(expr);
            }

            Expr::Sizeof(arg) => {
                self.write("sizeof(");
                match arg {
                    SizeofArg::Type(t) => self.emit_type(t),
                    SizeofArg::Expr(e) => self.emit_expr(e),
                }
                self.write(")");
            }

            Expr::Ternary { condition, then_expr, else_expr } => {
                self.write("(");
                self.emit_expr(condition);
                self.write(" ? ");
                self.emit_expr(then_expr);
                self.write(" : ");
                self.emit_expr(else_expr);
                self.write(")");
            }

            Expr::Comma(exprs) => {
                self.write("(");
                for (i, e) in exprs.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.emit_expr(e);
                }
                self.write(")");
            }

            Expr::InitList(exprs) => {
                self.write("{ ");
                for (i, e) in exprs.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.emit_expr(e);
                }
                self.write(" }");
            }

            Expr::DesignatedInit(fields) => {
                self.write("{ ");
                for (i, (name, value)) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(".");
                    self.write(name);
                    self.write(" = ");
                    self.emit_expr(value);
                }
                self.write(" }");
            }

            Expr::Unwrap(inner) => {
                // Access ok_value field (works for both Option and Result now)
                self.write("((");
                self.emit_expr(inner);
                self.write(").ok_value)");
            }

            Expr::UnwrapOr { expr, default } => {
                self.write("((");
                self.emit_expr(expr);
                self.write(").is_some ? (");
                self.emit_expr(expr);
                self.write(").ok_value : (");
                self.emit_expr(default);
                self.write("))");
            }
            
            Expr::IsSome(inner) => {
                self.write("((");
                self.emit_expr(inner);
                self.write(").is_some)");
            }
            
            Expr::IsNone(inner) => {
                self.write("(!(");
                self.emit_expr(inner);
                self.write(").is_some)");
            }
            
            Expr::Ok(value) => {
                // Get the current return type to determine the Result type name
                if let Some(ref ret_type) = self.current_return_type.clone() {
                    if let BaseType::Result(_, _) = &ret_type.base {
                        let type_name = self.result_type_name_from_spec(&ret_type);
                        self.write(&format!("{}_OK(", type_name));
                        self.emit_expr(value);
                        self.write(")");
                    } else {
                        // Fallback
                        self.write("/* ok */ (");
                        self.emit_expr(value);
                        self.write(")");
                    }
                } else {
                    self.write("/* ok */ (");
                    self.emit_expr(value);
                    self.write(")");
                }
            }
            
            Expr::Err(value) => {
                if let Some(ref ret_type) = self.current_return_type.clone() {
                    if let BaseType::Result(_, _) = &ret_type.base {
                        let type_name = self.result_type_name_from_spec(&ret_type);
                        self.write(&format!("{}_ERR(", type_name));
                        self.emit_expr(value);
                        self.write(")");
                    } else {
                        self.write("/* err */ (");
                        self.emit_expr(value);
                        self.write(")");
                    }
                } else {
                    self.write("/* err */ (");
                    self.emit_expr(value);
                    self.write(")");
                }
            }
            
            Expr::Try { expr, .. } => {
                // The ? operator is handled specially in emit_stmt for statements
                // For expressions, we emit a placeholder that will be expanded
                // This case handles when ? is used in a non-statement context
                self.write("/* try */ (");
                self.emit_expr(expr);
                self.write(").ok_value");
            }
            
            Expr::IsOk(inner) => {
                self.write("((");
                self.emit_expr(inner);
                self.write(").is_ok)");
            }
            
            Expr::IsErr(inner) => {
                self.write("(!(");
                self.emit_expr(inner);
                self.write(").is_ok)");
            }
            
            Expr::UnwrapErr(inner) => {
                self.write("((");
                self.emit_expr(inner);
                self.write(").err_value)");
            }
        }
    }

    // === Helper methods ===

    fn write(&mut self, s: &str) {
        self.output.push_str(s);
    }

    fn writeln(&mut self, s: &str) {
        self.output.push_str(s);
        self.output.push('\n');
    }

    fn newline(&mut self) {
        self.output.push('\n');
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str("    ");
        }
    }
}

impl Default for CEmitter {
    fn default() -> Self {
        Self::new()
    }
}

/// Escape a string for C output
fn escape_string(s: &str) -> String {
    let mut result = String::new();
    for c in s.chars() {
        match c {
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            '\\' => result.push_str("\\\\"),
            '"' => result.push_str("\\\""),
            c if c.is_ascii_control() => {
                result.push_str(&format!("\\x{:02x}", c as u32));
            }
            c => result.push(c),
        }
    }
    result
}

/// Escape a char for C output
fn escape_char(c: char) -> String {
    match c {
        '\n' => "\\n".to_string(),
        '\r' => "\\r".to_string(),
        '\t' => "\\t".to_string(),
        '\\' => "\\\\".to_string(),
        '\'' => "\\'".to_string(),
        '\0' => "\\0".to_string(),
        c if c.is_ascii_control() => format!("\\x{:02x}", c as u32),
        c => c.to_string(),
    }
}
