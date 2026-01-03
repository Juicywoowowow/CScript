//! Diagnostic reporting system for rich, actionable error messages.

mod reporter;

pub use reporter::DiagnosticReporter;

use colored::Colorize;
use std::fmt;

/// Location in source code
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub length: usize,
}

impl SourceLocation {
    pub fn new(file: &str, line: usize, column: usize, length: usize) -> Self {
        Self {
            file: file.to_string(),
            line,
            column,
            length,
        }
    }
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)
    }
}

/// Severity level of a diagnostic
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Note,
    Help,
}

impl fmt::Display for DiagnosticLevel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagnosticLevel::Error => write!(f, "{}", "error".red().bold()),
            DiagnosticLevel::Warning => write!(f, "{}", "warning".yellow().bold()),
            DiagnosticLevel::Note => write!(f, "{}", "note".cyan().bold()),
            DiagnosticLevel::Help => write!(f, "{}", "help".green().bold()),
        }
    }
}

/// A compiler diagnostic with rich context
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub code: String,
    pub message: String,
    pub location: Option<SourceLocation>,
    pub source_line: Option<String>,
    pub labels: Vec<(usize, usize, String)>, // (column, length, label)
    pub help: Option<String>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    pub fn error(code: &str, message: impl Into<String>) -> Self {
        Self {
            level: DiagnosticLevel::Error,
            code: code.to_string(),
            message: message.into(),
            location: None,
            source_line: None,
            labels: Vec::new(),
            help: None,
            notes: Vec::new(),
        }
    }

    pub fn warning(code: &str, message: impl Into<String>) -> Self {
        Self {
            level: DiagnosticLevel::Warning,
            code: code.to_string(),
            message: message.into(),
            location: None,
            source_line: None,
            labels: Vec::new(),
            help: None,
            notes: Vec::new(),
        }
    }

    pub fn with_location(mut self, location: SourceLocation) -> Self {
        self.location = Some(location);
        self
    }

    pub fn with_source_line(mut self, line: impl Into<String>) -> Self {
        self.source_line = Some(line.into());
        self
    }

    pub fn with_label(mut self, column: usize, length: usize, label: impl Into<String>) -> Self {
        self.labels.push((column, length, label.into()));
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn is_error(&self) -> bool {
        matches!(self.level, DiagnosticLevel::Error)
    }
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Header: error[E001]: message
        writeln!(f, "{}[{}]: {}", self.level, self.code.cyan(), self.message)?;

        // Location arrow
        if let Some(ref loc) = self.location {
            let line_num_width = loc.line.to_string().len();
            let padding = " ".repeat(line_num_width);

            writeln!(f, "{}--> {}", padding, loc.to_string().blue())?;
            writeln!(f, "{} {}", padding, "|".blue())?;

            // Source line with line number
            if let Some(ref source) = self.source_line {
                writeln!(f, "{} {} {}", 
                    loc.line.to_string().blue().bold(),
                    "|".blue(),
                    source
                )?;

                // Underlines and labels
                for (column, length, label) in &self.labels {
                    let underline_padding = " ".repeat(column.saturating_sub(1));
                    let underline = "^".repeat(*length);
                    
                    let colored_underline = match self.level {
                        DiagnosticLevel::Error => underline.red().bold().to_string(),
                        DiagnosticLevel::Warning => underline.yellow().bold().to_string(),
                        _ => underline.cyan().to_string(),
                    };

                    let colored_label = match self.level {
                        DiagnosticLevel::Error => label.red().to_string(),
                        DiagnosticLevel::Warning => label.yellow().to_string(),
                        _ => label.cyan().to_string(),
                    };

                    writeln!(f, "{} {} {}{} {}", 
                        padding,
                        "|".blue(),
                        underline_padding,
                        colored_underline,
                        colored_label
                    )?;
                }
            }

            writeln!(f, "{} {}", padding, "|".blue())?;
        }

        // Help message
        if let Some(ref help) = self.help {
            writeln!(f, "   {} {}: {}", "=".blue(), "help".green().bold(), help)?;
        }

        // Notes
        for note in &self.notes {
            writeln!(f, "   {} {}: {}", "=".blue(), "note".cyan().bold(), note)?;
        }

        Ok(())
    }
}

/// Error codes for CScript diagnostics
pub mod codes {
    // Lexer errors (E0xx)
    pub const UNEXPECTED_CHARACTER: &str = "E001";
    pub const UNTERMINATED_STRING: &str = "E002";
    pub const UNTERMINATED_CHAR: &str = "E003";
    pub const INVALID_ESCAPE: &str = "E004";
    pub const INVALID_NUMBER: &str = "E005";

    // Parser errors (E1xx)
    pub const EXPECTED_TOKEN: &str = "E100";
    pub const EXPECTED_EXPRESSION: &str = "E101";
    pub const EXPECTED_STATEMENT: &str = "E102";
    pub const EXPECTED_TYPE: &str = "E103";
    pub const EXPECTED_IDENTIFIER: &str = "E104";
    pub const UNEXPECTED_TOKEN: &str = "E105";
    pub const UNCLOSED_PAREN: &str = "E106";
    pub const UNCLOSED_BRACE: &str = "E107";
    pub const UNCLOSED_BRACKET: &str = "E108";

    // Type errors (E2xx)
    pub const TYPE_MISMATCH: &str = "E200";
    pub const IMPLICIT_CONVERSION: &str = "E201";
    pub const INVALID_CAST: &str = "E202";
    pub const NON_NULLABLE_ASSIGN_NONE: &str = "E203";
    pub const UNINITIALIZED_VARIABLE: &str = "E204";
    pub const UNDEFINED_VARIABLE: &str = "E205";
    pub const UNDEFINED_FUNCTION: &str = "E206";
    pub const UNDEFINED_TYPE: &str = "E207";
    pub const ARGUMENT_COUNT_MISMATCH: &str = "E208";
    pub const RETURN_TYPE_MISMATCH: &str = "E209";
    pub const VOID_VARIABLE: &str = "E210";

    // Mutability errors (E3xx)
    pub const MUTATE_IMMUTABLE: &str = "E300";
    pub const MUTABLE_BORROW_IMMUTABLE: &str = "E301";

    // Array/Pointer errors (E4xx)
    pub const INDEX_OUT_OF_BOUNDS: &str = "E400";
    pub const INDEX_NON_ARRAY: &str = "E401";
    pub const DEREFERENCE_NON_POINTER: &str = "E402";
    pub const ADDRESS_OF_RVALUE: &str = "E403";

    // Warning codes (W0xx)
    pub const UNUSED_VARIABLE: &str = "W001";
    pub const UNUSED_FUNCTION: &str = "W002";
    pub const SHADOWED_VARIABLE: &str = "W003";
    pub const UNREACHABLE_CODE: &str = "W004";
    pub const NARROWING_CONVERSION: &str = "W005";
    pub const SIGNED_UNSIGNED_COMPARE: &str = "W006";
}
