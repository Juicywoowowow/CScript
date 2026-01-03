//! Parser module for building AST from tokens.

mod ast;
mod expression;
mod statement;

pub use ast::*;
pub use expression::ExpressionParser;
pub use statement::StatementParser;

use crate::diagnostics::{codes, Diagnostic, DiagnosticReporter};
use crate::lexer::{Token, TokenKind};

/// Recursive descent parser for CScript
pub struct Parser<'a> {
    tokens: Vec<Token>,
    current: usize,
    reporter: &'a mut DiagnosticReporter,
    panic_mode: bool,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, reporter: &'a mut DiagnosticReporter) -> Self {
        Self {
            tokens,
            current: 0,
            reporter,
            panic_mode: false,
        }
    }

    /// Parse the entire program
    pub fn parse(&mut self) -> Program {
        let mut declarations = Vec::new();

        while !self.is_at_end() {
            match self.declaration() {
                Some(decl) => declarations.push(decl),
                None => {
                    // Error recovery: skip to next sync point
                    self.synchronize();
                }
            }
        }

        Program { declarations }
    }

    /// Parse a top-level declaration
    fn declaration(&mut self) -> Option<Declaration> {
        // Preprocessor directives pass through
        if self.check(TokenKind::Preprocessor) {
            let token = self.advance();
            return Some(Declaration::Preprocessor(token.lexeme.clone()));
        }

        // Storage class specifiers
        let is_static = self.match_token(TokenKind::Static);
        let is_extern = self.match_token(TokenKind::Extern);
        let is_inline = self.match_token(TokenKind::Inline);

        // Type definitions
        if self.check(TokenKind::Typedef) {
            return self.typedef_declaration();
        }

        if self.check(TokenKind::Struct) {
            return self.struct_declaration(is_static);
        }

        if self.check(TokenKind::Enum) {
            return self.enum_declaration();
        }

        if self.check(TokenKind::Union) {
            return self.union_declaration();
        }

        // Variable or function declaration
        self.var_or_func_declaration(is_static, is_extern, is_inline)
    }

    /// Parse variable or function declaration
    fn var_or_func_declaration(
        &mut self,
        is_static: bool,
        is_extern: bool,
        is_inline: bool,
    ) -> Option<Declaration> {
        // Check for 'mut' keyword
        let is_mutable = self.match_token(TokenKind::Mut);

        // Parse type
        let type_spec = self.parse_type()?;

        // Parse declarator (name and possible pointer/array modifiers)
        let (name, name_span) = self.expect_identifier_with_span("expected identifier after type")?;

        // Function declaration
        if self.check(TokenKind::LeftParen) {
            return self.function_declaration(type_spec, name, name_span, is_static, is_extern, is_inline);
        }

        // Variable declaration
        self.variable_declaration(type_spec, name, name_span, is_mutable, is_static, is_extern)
    }

    /// Parse function declaration
    fn function_declaration(
        &mut self,
        return_type: TypeSpec,
        name: String,
        name_span: Span,
        is_static: bool,
        is_extern: bool,
        is_inline: bool,
    ) -> Option<Declaration> {
        self.expect(TokenKind::LeftParen, "expected '(' after function name")?;

        let mut params = Vec::new();

        if !self.check(TokenKind::RightParen) {
            loop {
                // Check for 'mut' on parameter
                let is_mutable = self.match_token(TokenKind::Mut);
                let param_type = self.parse_type()?;
                
                // Get parameter name with span if present
                let (param_name, param_span) = if self.check(TokenKind::Identifier) {
                    let (n, s) = self.expect_identifier_with_span("expected parameter name")?;
                    (Some(n), Some(s))
                } else {
                    (None, None)
                };

                params.push(Parameter {
                    type_spec: param_type,
                    name: param_name,
                    name_span: param_span,
                    is_mutable,
                });

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.expect(TokenKind::RightParen, "expected ')' after parameters")?;

        // Function body or declaration only
        let body = if self.check(TokenKind::LeftBrace) {
            Some(self.block()?)
        } else {
            self.expect(TokenKind::Semicolon, "expected ';' after function declaration")?;
            None
        };

        Some(Declaration::Function(FunctionDecl {
            return_type,
            name,
            name_span,
            params,
            body,
            is_static,
            is_extern,
            is_inline,
        }))
    }

    /// Parse variable declaration
    fn variable_declaration(
        &mut self,
        type_spec: TypeSpec,
        name: String,
        name_span: Span,
        is_mutable: bool,
        is_static: bool,
        is_extern: bool,
    ) -> Option<Declaration> {
        // Check for array dimensions
        let array_dims = self.parse_array_dimensions();

        // Check for initialization
        let initializer = if self.match_token(TokenKind::Equal) {
            Some(self.expression()?)
        } else {
            None
        };

        self.expect(TokenKind::Semicolon, "expected ';' after variable declaration")?;

        Some(Declaration::Variable(VariableDecl {
            type_spec,
            name,
            name_span,
            array_dims,
            initializer,
            is_mutable,
            is_static,
            is_extern,
        }))
    }

    /// Parse a type specifier
    fn parse_type(&mut self) -> Option<TypeSpec> {
        let mut is_const = false;
        let mut is_volatile = false;

        // Type qualifiers
        loop {
            if self.match_token(TokenKind::Volatile) {
                is_volatile = true;
            } else {
                break;
            }
        }

        // Option<T> type
        if self.match_token(TokenKind::Option) {
            self.expect(TokenKind::Less, "expected '<' after Option")?;
            let inner = self.parse_type()?;
            self.expect(TokenKind::Greater, "expected '>' after Option type")?;
            
            return Some(TypeSpec {
                base: BaseType::Option(Box::new(inner)),
                pointer_depth: 0,
                is_const,
                is_volatile,
            });
        }
        
        // Result<T, E> type
        if self.match_token(TokenKind::Result) {
            self.expect(TokenKind::Less, "expected '<' after Result")?;
            let ok_type = self.parse_type()?;
            self.expect(TokenKind::Comma, "expected ',' between Result types")?;
            let err_type = self.parse_type()?;
            self.expect(TokenKind::Greater, "expected '>' after Result types")?;
            
            return Some(TypeSpec {
                base: BaseType::Result(Box::new(ok_type), Box::new(err_type)),
                pointer_depth: 0,
                is_const,
                is_volatile,
            });
        }

        // Base type
        let base = self.parse_base_type()?;

        // Pointer modifiers
        let mut pointer_depth = 0;
        while self.match_token(TokenKind::Star) {
            pointer_depth += 1;
        }

        Some(TypeSpec {
            base,
            pointer_depth,
            is_const,
            is_volatile,
        })
    }

    /// Parse base type (int, float, struct name, etc.)
    fn parse_base_type(&mut self) -> Option<BaseType> {
        let token = self.peek();

        let base = match token.kind {
            // CScript integer types
            TokenKind::Int8 => { self.advance(); BaseType::Int8 }
            TokenKind::Int16 => { self.advance(); BaseType::Int16 }
            TokenKind::Int32 => { self.advance(); BaseType::Int32 }
            TokenKind::Int64 => { self.advance(); BaseType::Int64 }
            TokenKind::Uint8 => { self.advance(); BaseType::Uint8 }
            TokenKind::Uint16 => { self.advance(); BaseType::Uint16 }
            TokenKind::Uint32 => { self.advance(); BaseType::Uint32 }
            TokenKind::Uint64 => { self.advance(); BaseType::Uint64 }
            
            // CScript float types
            TokenKind::Float32 => { self.advance(); BaseType::Float32 }
            TokenKind::Float64 => { self.advance(); BaseType::Float64 }
            
            // C-compatible types
            TokenKind::Int => { self.advance(); BaseType::Int }
            TokenKind::Long => { self.advance(); BaseType::Long }
            TokenKind::Short => { self.advance(); BaseType::Short }
            TokenKind::Float => { self.advance(); BaseType::Float }
            TokenKind::Double => { self.advance(); BaseType::Double }
            TokenKind::Char => { self.advance(); BaseType::Char }
            TokenKind::Void => { self.advance(); BaseType::Void }
            TokenKind::Bool => { self.advance(); BaseType::Bool }
            
            // Unsigned modifier
            TokenKind::Unsigned => {
                self.advance();
                if self.match_token(TokenKind::Int) {
                    BaseType::UnsignedInt
                } else if self.match_token(TokenKind::Long) {
                    BaseType::UnsignedLong
                } else if self.match_token(TokenKind::Short) {
                    BaseType::UnsignedShort
                } else if self.match_token(TokenKind::Char) {
                    BaseType::UnsignedChar
                } else {
                    BaseType::UnsignedInt // default
                }
            }
            
            // Signed modifier
            TokenKind::Signed => {
                self.advance();
                if self.match_token(TokenKind::Int) {
                    BaseType::Int
                } else if self.match_token(TokenKind::Long) {
                    BaseType::Long
                } else if self.match_token(TokenKind::Short) {
                    BaseType::Short
                } else if self.match_token(TokenKind::Char) {
                    BaseType::Char
                } else {
                    BaseType::Int // default
                }
            }
            
            // Struct type
            TokenKind::Struct => {
                self.advance();
                let name = self.expect_identifier("expected struct name")?;
                BaseType::Struct(name)
            }
            
            // Enum type
            TokenKind::Enum => {
                self.advance();
                let name = self.expect_identifier("expected enum name")?;
                BaseType::Enum(name)
            }
            
            // Union type
            TokenKind::Union => {
                self.advance();
                let name = self.expect_identifier("expected union name")?;
                BaseType::Union(name)
            }
            
            // Named type (typedef)
            TokenKind::Identifier => {
                let name = self.advance().lexeme.clone();
                BaseType::Named(name)
            }
            
            _ => {
                self.error_at_current(
                    codes::EXPECTED_TYPE,
                    &format!("expected type, found '{}'", token.lexeme),
                );
                return None;
            }
        };

        Some(base)
    }

    /// Parse array dimensions: [10], [5][3], []
    fn parse_array_dimensions(&mut self) -> Vec<Option<Expr>> {
        let mut dims = Vec::new();

        while self.match_token(TokenKind::LeftBracket) {
            if self.check(TokenKind::RightBracket) {
                dims.push(None);
            } else if let Some(expr) = self.expression() {
                dims.push(Some(expr));
            }
            if !self.match_token(TokenKind::RightBracket) {
                self.error_at_current(codes::EXPECTED_TOKEN, "expected ']' after array dimension");
                break;
            }
        }

        dims
    }

    /// Parse a block of statements
    fn block(&mut self) -> Option<Block> {
        self.expect(TokenKind::LeftBrace, "expected '{'")?;

        let mut statements = Vec::new();

        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            match self.statement() {
                Some(stmt) => statements.push(stmt),
                None => {
                    self.synchronize();
                }
            }
        }

        self.expect(TokenKind::RightBrace, "expected '}'")?;

        Some(Block { statements })
    }

    /// Parse struct declaration
    fn struct_declaration(&mut self, is_static: bool) -> Option<Declaration> {
        self.advance(); // consume 'struct'
        
        let name = if self.check(TokenKind::Identifier) {
            Some(self.advance().lexeme.clone())
        } else {
            None
        };

        if !self.check(TokenKind::LeftBrace) {
            // Forward declaration or variable using struct type
            self.expect(TokenKind::Semicolon, "expected ';' after struct declaration")?;
            return Some(Declaration::Struct(StructDecl {
                name,
                fields: Vec::new(),
            }));
        }

        self.expect(TokenKind::LeftBrace, "expected '{' for struct body")?;

        let mut fields = Vec::new();

        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            let is_mutable = self.match_token(TokenKind::Mut);
            let field_type = self.parse_type()?;
            let field_name = self.expect_identifier("expected field name")?;
            let array_dims = self.parse_array_dimensions();
            self.expect(TokenKind::Semicolon, "expected ';' after field")?;

            fields.push(StructField {
                type_spec: field_type,
                name: field_name,
                array_dims,
                is_mutable,
            });
        }

        self.expect(TokenKind::RightBrace, "expected '}' after struct fields")?;
        self.expect(TokenKind::Semicolon, "expected ';' after struct definition")?;

        Some(Declaration::Struct(StructDecl { name, fields }))
    }

    /// Parse enum declaration
    fn enum_declaration(&mut self) -> Option<Declaration> {
        self.advance(); // consume 'enum'
        
        let name = if self.check(TokenKind::Identifier) {
            Some(self.advance().lexeme.clone())
        } else {
            None
        };

        if !self.check(TokenKind::LeftBrace) {
            self.expect(TokenKind::Semicolon, "expected ';' after enum declaration")?;
            return Some(Declaration::Enum(EnumDecl {
                name,
                variants: Vec::new(),
            }));
        }

        self.expect(TokenKind::LeftBrace, "expected '{' for enum body")?;

        let mut variants = Vec::new();

        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            let variant_name = self.expect_identifier("expected enum variant name")?;
            
            let value = if self.match_token(TokenKind::Equal) {
                Some(self.expression()?)
            } else {
                None
            };

            variants.push(EnumVariant {
                name: variant_name,
                value,
            });

            if !self.match_token(TokenKind::Comma) {
                break;
            }
        }

        self.expect(TokenKind::RightBrace, "expected '}' after enum variants")?;
        self.expect(TokenKind::Semicolon, "expected ';' after enum definition")?;

        Some(Declaration::Enum(EnumDecl { name, variants }))
    }

    /// Parse union declaration
    fn union_declaration(&mut self) -> Option<Declaration> {
        self.advance(); // consume 'union'
        
        let name = if self.check(TokenKind::Identifier) {
            Some(self.advance().lexeme.clone())
        } else {
            None
        };

        if !self.check(TokenKind::LeftBrace) {
            self.expect(TokenKind::Semicolon, "expected ';' after union declaration")?;
            return Some(Declaration::Union(UnionDecl {
                name,
                fields: Vec::new(),
            }));
        }

        self.expect(TokenKind::LeftBrace, "expected '{' for union body")?;

        let mut fields = Vec::new();

        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            let field_type = self.parse_type()?;
            let field_name = self.expect_identifier("expected field name")?;
            self.expect(TokenKind::Semicolon, "expected ';' after field")?;

            fields.push(UnionField {
                type_spec: field_type,
                name: field_name,
            });
        }

        self.expect(TokenKind::RightBrace, "expected '}' after union fields")?;
        self.expect(TokenKind::Semicolon, "expected ';' after union definition")?;

        Some(Declaration::Union(UnionDecl { name, fields }))
    }

    /// Parse typedef declaration
    fn typedef_declaration(&mut self) -> Option<Declaration> {
        self.advance(); // consume 'typedef'
        
        let original_type = self.parse_type()?;
        let new_name = self.expect_identifier("expected typedef name")?;
        self.expect(TokenKind::Semicolon, "expected ';' after typedef")?;

        Some(Declaration::Typedef(TypedefDecl {
            original_type,
            new_name,
        }))
    }

    // === Helper methods ===

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn check(&self, kind: TokenKind) -> bool {
        self.peek().kind == kind
    }

    fn match_token(&mut self, kind: TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind, message: &str) -> Option<&Token> {
        if self.check(kind) {
            Some(self.advance())
        } else {
            self.error_at_current(codes::EXPECTED_TOKEN, message);
            None
        }
    }

    fn expect_identifier(&mut self, message: &str) -> Option<String> {
        if self.check(TokenKind::Identifier) {
            Some(self.advance().lexeme.clone())
        } else {
            self.error_at_current(codes::EXPECTED_IDENTIFIER, message);
            None
        }
    }

    fn expect_identifier_with_span(&mut self, message: &str) -> Option<(String, Span)> {
        if self.check(TokenKind::Identifier) {
            let token = self.advance();
            Some((token.lexeme.clone(), Span::new(token.offset, token.length)))
        } else {
            self.error_at_current(codes::EXPECTED_IDENTIFIER, message);
            None
        }
    }

    fn error_at_current(&mut self, code: &str, message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;

        let token = self.peek();
        let diag = Diagnostic::error(code, message);
        
        self.reporter.report(diag, token.offset, token.length);
    }

    /// Error recovery: skip tokens until we find a synchronization point
    fn synchronize(&mut self) {
        self.panic_mode = false;

        while !self.is_at_end() {
            // After a semicolon, we're likely at the start of a new statement
            if self.previous().kind == TokenKind::Semicolon {
                return;
            }

            // Before certain keywords, we're likely at the start of a new declaration
            match self.peek().kind {
                TokenKind::Struct
                | TokenKind::Enum
                | TokenKind::Union
                | TokenKind::Typedef
                | TokenKind::Static
                | TokenKind::Extern
                | TokenKind::If
                | TokenKind::While
                | TokenKind::For
                | TokenKind::Return
                | TokenKind::Int
                | TokenKind::Void
                | TokenKind::Char
                | TokenKind::Float
                | TokenKind::Double
                | TokenKind::Bool
                | TokenKind::Preprocessor => {
                    return;
                }
                _ => {}
            }

            self.advance();
        }
    }
}
