//! Diagnostic reporter that collects and manages diagnostics.

use super::{Diagnostic, SourceLocation};

/// Collects diagnostics during compilation
#[derive(Debug, Default)]
pub struct DiagnosticReporter {
    diagnostics: Vec<Diagnostic>,
    source: String,
    file: String,
    lines: Vec<(usize, usize)>, // (start, end) byte offsets for each line
}

impl DiagnosticReporter {
    pub fn new(file: &str, source: &str) -> Self {
        let mut lines = Vec::new();
        let mut start = 0;

        for (i, c) in source.char_indices() {
            if c == '\n' {
                lines.push((start, i));
                start = i + 1;
            }
        }

        // Handle last line (no trailing newline)
        if start <= source.len() {
            lines.push((start, source.len()));
        }

        Self {
            diagnostics: Vec::new(),
            source: source.to_string(),
            file: file.to_string(),
            lines,
        }
    }

    /// Get line content and location from byte offset
    pub fn location_from_offset(&self, offset: usize) -> (SourceLocation, String) {
        let mut line_num = 1;
        let mut line_start = 0;

        for (i, &(start, end)) in self.lines.iter().enumerate() {
            if offset >= start && offset <= end {
                line_num = i + 1;
                line_start = start;
                break;
            }
        }

        let column = offset - line_start + 1;
        let line_content = self.get_line(line_num);

        (
            SourceLocation::new(&self.file, line_num, column, 1),
            line_content,
        )
    }

    /// Get a specific line's content
    pub fn get_line(&self, line_num: usize) -> String {
        if line_num == 0 || line_num > self.lines.len() {
            return String::new();
        }

        let (start, end) = self.lines[line_num - 1];
        self.source[start..end].to_string()
    }

    /// Report a diagnostic with automatic source line lookup
    pub fn report(&mut self, mut diagnostic: Diagnostic, offset: usize, length: usize) {
        let (mut loc, line_content) = self.location_from_offset(offset);
        loc.length = length;

        diagnostic = diagnostic
            .with_location(loc.clone())
            .with_source_line(line_content)
            .with_label(loc.column, length, "");

        self.diagnostics.push(diagnostic);
    }

    /// Report a diagnostic with custom label
    pub fn report_with_label(
        &mut self, 
        mut diagnostic: Diagnostic, 
        offset: usize, 
        length: usize,
        label: &str
    ) {
        let (mut loc, line_content) = self.location_from_offset(offset);
        loc.length = length;

        diagnostic = diagnostic
            .with_location(loc.clone())
            .with_source_line(line_content)
            .with_label(loc.column, length, label);

        self.diagnostics.push(diagnostic);
    }

    /// Add a raw diagnostic (already formatted)
    pub fn add(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.is_error())
    }

    /// Get error count
    pub fn error_count(&self) -> usize {
        self.diagnostics.iter().filter(|d| d.is_error()).count()
    }

    /// Consume and return all diagnostics
    pub fn take_diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics
    }

    /// Get reference to diagnostics
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
}
