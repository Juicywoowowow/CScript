//! Scanner for CScript source code tokenization.

use super::token::{lookup_keyword, Token, TokenKind};
use crate::diagnostics::{codes, Diagnostic, DiagnosticReporter};

/// Scanner that produces tokens from source code
pub struct Scanner<'a> {
    source: &'a str,
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    current_offset: usize,
    start_offset: usize,
    reporter: &'a mut DiagnosticReporter,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str, reporter: &'a mut DiagnosticReporter) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            current_offset: 0,
            start_offset: 0,
            reporter,
        }
    }

    /// Tokenize the entire source
    pub fn scan_tokens(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        loop {
            let token = self.scan_token();
            let is_eof = token.kind == TokenKind::Eof;
            
            if token.kind != TokenKind::Error {
                tokens.push(token);
            }
            
            if is_eof {
                break;
            }
        }

        tokens
    }

    /// Scan a single token
    fn scan_token(&mut self) -> Token {
        self.skip_whitespace_and_comments();
        self.start_offset = self.current_offset;

        match self.advance() {
            None => Token::eof(self.current_offset),
            Some((offset, c)) => {
                self.start_offset = offset;
                self.current_offset = offset + c.len_utf8();

                match c {
                    // Single-char tokens
                    '(' => self.make_token(TokenKind::LeftParen),
                    ')' => self.make_token(TokenKind::RightParen),
                    '{' => self.make_token(TokenKind::LeftBrace),
                    '}' => self.make_token(TokenKind::RightBrace),
                    '[' => self.make_token(TokenKind::LeftBracket),
                    ']' => self.make_token(TokenKind::RightBracket),
                    ',' => self.make_token(TokenKind::Comma),
                    ';' => self.make_token(TokenKind::Semicolon),
                    '~' => self.make_token(TokenKind::Tilde),
                    '?' => self.make_token(TokenKind::Question),
                    
                    // Potentially compound tokens
                    '+' => self.match_compound(&[
                        ('+', TokenKind::PlusPlus),
                        ('=', TokenKind::PlusEqual),
                    ], TokenKind::Plus),
                    
                    '-' => self.match_compound(&[
                        ('-', TokenKind::MinusMinus),
                        ('=', TokenKind::MinusEqual),
                        ('>', TokenKind::Arrow),
                    ], TokenKind::Minus),
                    
                    '*' => self.match_compound(&[
                        ('=', TokenKind::StarEqual),
                    ], TokenKind::Star),
                    
                    '/' => self.match_compound(&[
                        ('=', TokenKind::SlashEqual),
                    ], TokenKind::Slash),
                    
                    '%' => self.match_compound(&[
                        ('=', TokenKind::PercentEqual),
                    ], TokenKind::Percent),
                    
                    '&' => self.match_compound(&[
                        ('&', TokenKind::AmpersandAmpersand),
                        ('=', TokenKind::AmpersandEqual),
                    ], TokenKind::Ampersand),
                    
                    '|' => self.match_compound(&[
                        ('|', TokenKind::PipePipe),
                        ('=', TokenKind::PipeEqual),
                    ], TokenKind::Pipe),
                    
                    '^' => self.match_compound(&[
                        ('=', TokenKind::CaretEqual),
                    ], TokenKind::Caret),
                    
                    '!' => self.match_compound(&[
                        ('=', TokenKind::BangEqual),
                    ], TokenKind::Bang),
                    
                    '=' => self.match_compound(&[
                        ('=', TokenKind::EqualEqual),
                        ('>', TokenKind::FatArrow),
                    ], TokenKind::Equal),
                    
                    '<' => {
                        if self.match_char('<') {
                            if self.match_char('=') {
                                self.make_token(TokenKind::LessLessEqual)
                            } else {
                                self.make_token(TokenKind::LessLess)
                            }
                        } else if self.match_char('=') {
                            self.make_token(TokenKind::LessEqual)
                        } else {
                            self.make_token(TokenKind::Less)
                        }
                    }
                    
                    '>' => {
                        if self.match_char('>') {
                            if self.match_char('=') {
                                self.make_token(TokenKind::GreaterGreaterEqual)
                            } else {
                                self.make_token(TokenKind::GreaterGreater)
                            }
                        } else if self.match_char('=') {
                            self.make_token(TokenKind::GreaterEqual)
                        } else {
                            self.make_token(TokenKind::Greater)
                        }
                    }
                    
                    ':' => self.match_compound(&[
                        (':', TokenKind::ColonColon),
                    ], TokenKind::Colon),
                    
                    '.' => {
                        // Check for number starting with .
                        if self.peek().map_or(false, |c| c.is_ascii_digit()) {
                            self.scan_number_after_dot()
                        } else {
                            self.make_token(TokenKind::Dot)
                        }
                    }
                    
                    // Preprocessor directives (pass-through)
                    '#' => self.scan_preprocessor(),
                    
                    // String literal
                    '"' => self.scan_string(),
                    
                    // Char literal
                    '\'' => self.scan_char(),
                    
                    // Numbers
                    '0'..='9' => self.scan_number(c),
                    
                    // Identifiers and keywords
                    c if is_ident_start(c) => self.scan_identifier(c),
                    
                    // Unknown character
                    _ => self.error_token(c),
                }
            }
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek() {
                Some(' ' | '\t' | '\r' | '\n') => {
                    self.advance();
                }
                Some('/') => {
                    // Look ahead for comment
                    let mut chars = self.chars.clone();
                    chars.next(); // consume '/'
                    match chars.peek() {
                        Some((_, '/')) => {
                            // Line comment
                            self.advance(); // '/'
                            self.advance(); // '/'
                            while self.peek().map_or(false, |c| c != '\n') {
                                self.advance();
                            }
                        }
                        Some((_, '*')) => {
                            // Block comment
                            self.advance(); // '/'
                            self.advance(); // '*'
                            let mut depth = 1;
                            while depth > 0 {
                                match self.advance() {
                                    None => break,
                                    Some((_, '*')) => {
                                        if self.match_char('/') {
                                            depth -= 1;
                                        }
                                    }
                                    Some((_, '/')) => {
                                        if self.match_char('*') {
                                            depth += 1;
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                        _ => break,
                    }
                }
                _ => break,
            }
        }
    }

    fn advance(&mut self) -> Option<(usize, char)> {
        let result = self.chars.next();
        if let Some((_, c)) = result {
            self.current_offset += c.len_utf8();
        }
        result
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.peek() == Some(expected) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_compound(&mut self, options: &[(char, TokenKind)], default: TokenKind) -> Token {
        for (c, kind) in options {
            if self.match_char(*c) {
                return self.make_token(*kind);
            }
        }
        self.make_token(default)
    }

    fn make_token(&self, kind: TokenKind) -> Token {
        let lexeme = &self.source[self.start_offset..self.current_offset];
        Token::new(kind, lexeme, self.start_offset, self.current_offset - self.start_offset)
    }

    fn error_token(&mut self, c: char) -> Token {
        self.reporter.report(
            Diagnostic::error(codes::UNEXPECTED_CHARACTER, format!("unexpected character '{}'", c)),
            self.start_offset,
            c.len_utf8(),
        );
        Token::new(TokenKind::Error, c, self.start_offset, c.len_utf8())
    }

    fn scan_identifier(&mut self, first: char) -> Token {
        let mut ident = String::new();
        ident.push(first);

        while let Some(c) = self.peek() {
            if is_ident_continue(c) {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }

        let kind = lookup_keyword(&ident).unwrap_or(TokenKind::Identifier);
        self.make_token(kind)
    }

    fn scan_number(&mut self, first: char) -> Token {
        let mut is_float = false;

        // Check for hex, octal, binary
        if first == '0' {
            match self.peek() {
                Some('x' | 'X') => return self.scan_hex_number(),
                Some('b' | 'B') => return self.scan_binary_number(),
                Some('0'..='7') => return self.scan_octal_number(),
                _ => {}
            }
        }

        // Decimal digits
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        // Decimal point
        if self.peek() == Some('.') {
            // Look ahead to ensure it's a number and not a method call
            let mut chars = self.chars.clone();
            chars.next(); // consume '.'
            if chars.peek().map_or(false, |(_, c)| c.is_ascii_digit()) {
                self.advance(); // consume '.'
                is_float = true;
                
                while let Some(c) = self.peek() {
                    if c.is_ascii_digit() {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
        }

        // Exponent
        if let Some('e' | 'E') = self.peek() {
            self.advance();
            is_float = true;
            
            // Optional sign
            if let Some('+' | '-') = self.peek() {
                self.advance();
            }
            
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        // Type suffix (f, F, l, L, u, U, etc.)
        match self.peek() {
            Some('f' | 'F') => {
                self.advance();
                is_float = true;
            }
            Some('l' | 'L' | 'u' | 'U') => {
                self.advance();
                // Handle LL, UL, LU, etc.
                if let Some('l' | 'L' | 'u' | 'U') = self.peek() {
                    self.advance();
                }
            }
            _ => {}
        }

        self.make_token(if is_float { TokenKind::FloatLiteral } else { TokenKind::IntLiteral })
    }

    fn scan_number_after_dot(&mut self) -> Token {
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
            } else {
                break;
            }
        }

        // Exponent
        if let Some('e' | 'E') = self.peek() {
            self.advance();
            if let Some('+' | '-') = self.peek() {
                self.advance();
            }
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        if let Some('f' | 'F') = self.peek() {
            self.advance();
        }

        self.make_token(TokenKind::FloatLiteral)
    }

    fn scan_hex_number(&mut self) -> Token {
        self.advance(); // consume 'x'
        
        while let Some(c) = self.peek() {
            if c.is_ascii_hexdigit() {
                self.advance();
            } else {
                break;
            }
        }

        self.make_token(TokenKind::IntLiteral)
    }

    fn scan_binary_number(&mut self) -> Token {
        self.advance(); // consume 'b'
        
        while let Some(c) = self.peek() {
            if c == '0' || c == '1' {
                self.advance();
            } else {
                break;
            }
        }

        self.make_token(TokenKind::IntLiteral)
    }

    fn scan_octal_number(&mut self) -> Token {
        while let Some(c) = self.peek() {
            if c >= '0' && c <= '7' {
                self.advance();
            } else {
                break;
            }
        }

        self.make_token(TokenKind::IntLiteral)
    }

    fn scan_string(&mut self) -> Token {
        while let Some(c) = self.peek() {
            match c {
                '"' => {
                    self.advance();
                    return self.make_token(TokenKind::StringLiteral);
                }
                '\\' => {
                    self.advance(); // consume backslash
                    self.advance(); // consume escaped char
                }
                '\n' => {
                    self.reporter.report(
                        Diagnostic::error(codes::UNTERMINATED_STRING, "unterminated string literal")
                            .with_help("string literals cannot span multiple lines without escaping"),
                        self.start_offset,
                        self.current_offset - self.start_offset,
                    );
                    return Token::new(TokenKind::Error, "", self.start_offset, 0);
                }
                _ => {
                    self.advance();
                }
            }
        }

        self.reporter.report(
            Diagnostic::error(codes::UNTERMINATED_STRING, "unterminated string literal")
                .with_help("add a closing '\"' at the end of the string"),
            self.start_offset,
            self.current_offset - self.start_offset,
        );
        Token::new(TokenKind::Error, "", self.start_offset, 0)
    }

    fn scan_char(&mut self) -> Token {
        match self.advance() {
            None => {
                self.reporter.report(
                    Diagnostic::error(codes::UNTERMINATED_CHAR, "unterminated character literal"),
                    self.start_offset,
                    1,
                );
                return Token::new(TokenKind::Error, "", self.start_offset, 0);
            }
            Some((_, '\\')) => {
                // Escape sequence
                self.advance();
            }
            Some((_, '\'')) => {
                self.reporter.report(
                    Diagnostic::error(codes::UNTERMINATED_CHAR, "empty character literal"),
                    self.start_offset,
                    2,
                );
                return Token::new(TokenKind::Error, "", self.start_offset, 0);
            }
            _ => {}
        }

        if !self.match_char('\'') {
            self.reporter.report(
                Diagnostic::error(codes::UNTERMINATED_CHAR, "unterminated character literal")
                    .with_help("add a closing '\'' after the character"),
                self.start_offset,
                self.current_offset - self.start_offset,
            );
            return Token::new(TokenKind::Error, "", self.start_offset, 0);
        }

        self.make_token(TokenKind::CharLiteral)
    }

    fn scan_preprocessor(&mut self) -> Token {
        // Consume until end of line (handling line continuations)
        loop {
            match self.peek() {
                None | Some('\n') => break,
                Some('\\') => {
                    self.advance();
                    if self.peek() == Some('\n') {
                        self.advance(); // Skip line continuation
                    }
                }
                _ => {
                    self.advance();
                }
            }
        }

        self.make_token(TokenKind::Preprocessor)
    }
}

fn is_ident_start(c: char) -> bool {
    c == '_' || unicode_xid::UnicodeXID::is_xid_start(c)
}

fn is_ident_continue(c: char) -> bool {
    c == '_' || unicode_xid::UnicodeXID::is_xid_continue(c)
}
