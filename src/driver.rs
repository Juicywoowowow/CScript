//! Driver that orchestrates the compilation pipeline.

use crate::diagnostics::{Diagnostic, DiagnosticReporter};
use crate::lexer::Scanner;
use crate::parser::Parser;
use crate::semantic::TypeChecker;
use crate::codegen::CEmitter;

/// The compilation driver
pub struct Driver {
    file: String,
    source: String,
    bounds_checks: bool,
    dump_ast: bool,
    dump_tokens: bool,
}

impl Driver {
    pub fn new(file: String, source: String) -> Self {
        Self {
            file,
            source,
            bounds_checks: false,
            dump_ast: false,
            dump_tokens: false,
        }
    }

    pub fn set_bounds_checks(&mut self, enabled: bool) {
        self.bounds_checks = enabled;
    }

    pub fn set_dump_ast(&mut self, enabled: bool) {
        self.dump_ast = enabled;
    }

    pub fn set_dump_tokens(&mut self, enabled: bool) {
        self.dump_tokens = enabled;
    }

    /// Run the compilation pipeline
    pub fn compile(&mut self) -> Result<String, Vec<Diagnostic>> {
        let mut reporter = DiagnosticReporter::new(&self.file, &self.source);

        // === Lexical Analysis ===
        let scanner = Scanner::new(&self.source, &mut reporter);
        let tokens = scanner.scan_tokens();

        if self.dump_tokens {
            eprintln!("=== Tokens ===");
            for token in &tokens {
                eprintln!("  {:?}", token);
            }
            eprintln!();
        }

        if reporter.has_errors() {
            return Err(reporter.take_diagnostics());
        }

        // === Parsing ===
        let mut parser = Parser::new(tokens, &mut reporter);
        let program = parser.parse();

        if self.dump_ast {
            eprintln!("=== AST ===");
            eprintln!("{:#?}", program);
            eprintln!();
        }

        if reporter.has_errors() {
            return Err(reporter.take_diagnostics());
        }

        // === Semantic Analysis ===
        let mut type_checker = TypeChecker::new(&mut reporter);
        type_checker.check(&program);

        // Collect diagnostics (including warnings)
        let diagnostics = reporter.take_diagnostics();
        
        // Check for errors (warnings are allowed)
        let has_errors = diagnostics.iter().any(|d| d.is_error());
        
        if has_errors {
            return Err(diagnostics);
        }

        // Print warnings if any
        for diag in &diagnostics {
            eprintln!("{}", diag);
        }

        // === Code Generation ===
        let mut emitter = CEmitter::new();
        emitter.set_bounds_checks(self.bounds_checks);
        let c_code = emitter.emit(&program);

        Ok(c_code)
    }
}
