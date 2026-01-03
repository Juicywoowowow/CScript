//! Statement parser for CScript.

use super::{Block, Expr, MatchArm, MatchPattern, Parser, Span, Stmt, SwitchCase, VariableDecl};
use super::expression::ExpressionParser;
use crate::diagnostics::codes;
use crate::lexer::TokenKind;

/// Trait extension for statement parsing
pub trait StatementParser {
    fn statement(&mut self) -> Option<Stmt>;
}

impl<'a> StatementParser for Parser<'a> {
    fn statement(&mut self) -> Option<Stmt> {
        self.parse_statement()
    }
}

impl<'a> Parser<'a> {
    pub(crate) fn parse_statement(&mut self) -> Option<Stmt> {
        // Check for local variable declaration
        if self.is_declaration_start() {
            return self.local_var_decl();
        }

        match self.peek().kind {
            TokenKind::LeftBrace => {
                let block = self.block()?;
                Some(Stmt::Block(block))
            }

            TokenKind::If => self.if_statement(),
            TokenKind::While => self.while_statement(),
            TokenKind::Do => self.do_while_statement(),
            TokenKind::For => self.for_statement(),
            TokenKind::Switch => self.switch_statement(),
            TokenKind::Match => self.match_statement(),
            TokenKind::Return => self.return_statement(),
            TokenKind::Break => self.break_statement(),
            TokenKind::Continue => self.continue_statement(),
            TokenKind::Goto => self.goto_statement(),
            TokenKind::Semicolon => {
                self.advance();
                Some(Stmt::Empty)
            }

            // Check for label: statement
            TokenKind::Identifier => {
                // Look ahead for ':'
                if self.current + 1 < self.tokens.len() 
                    && self.tokens[self.current + 1].kind == TokenKind::Colon 
                {
                    let name = self.advance().lexeme.clone();
                    self.advance(); // consume ':'
                    return Some(Stmt::Label(name));
                }
                self.expression_statement()
            }

            _ => self.expression_statement(),
        }
    }

    /// Check if current position could start a declaration
    fn is_declaration_start(&self) -> bool {
        let kind = self.peek().kind;
        
        // Check for 'mut' keyword
        if kind == TokenKind::Mut {
            return true;
        }
        
        // Check for type keywords
        kind.can_start_declaration()
    }

    /// Parse local variable declaration (or struct declaration)
    fn local_var_decl(&mut self) -> Option<Stmt> {
        let is_mutable = self.match_token(TokenKind::Mut);
        let is_static = self.match_token(TokenKind::Static);
        
        // Check if this is a local struct definition
        if self.check(TokenKind::Struct) {
            // Peek ahead to see if it's a struct definition or a variable of struct type
            // struct Name { ... } vs struct Name varname;
            self.advance(); // consume 'struct'
            
            let struct_name = if self.check(TokenKind::Identifier) {
                Some(self.advance().lexeme.clone())
            } else {
                None
            };
            
            // If followed by '{', it's a struct definition
            if self.check(TokenKind::LeftBrace) {
                self.advance(); // consume '{'
                
                let mut fields = Vec::new();
                while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
                    let field_mutable = self.match_token(TokenKind::Mut);
                    let field_type = self.parse_type()?;
                    let field_name = self.expect_identifier("expected field name")?;
                    let array_dims = self.parse_array_dimensions();
                    self.expect(TokenKind::Semicolon, "expected ';' after field")?;
                    
                    fields.push(super::StructField {
                        type_spec: field_type,
                        name: field_name,
                        array_dims,
                        is_mutable: field_mutable,
                    });
                }
                
                self.expect(TokenKind::RightBrace, "expected '}' after struct fields")?;
                self.expect(TokenKind::Semicolon, "expected ';' after struct definition")?;
                
                return Some(Stmt::StructDecl(super::StructDecl {
                    name: struct_name,
                    fields,
                }));
            }
            
            // Otherwise it's a variable of struct type
            let type_spec = super::TypeSpec {
                base: super::BaseType::Struct(struct_name.unwrap_or_default()),
                pointer_depth: 0,
                is_const: false,
                is_volatile: false,
            };
            
            // Parse pointer modifiers
            let mut type_spec = type_spec;
            while self.match_token(TokenKind::Star) {
                type_spec.pointer_depth += 1;
            }
            
            let (name, name_span) = self.expect_identifier_with_span("expected variable name")?;
            let array_dims = self.parse_array_dimensions();
            
            let initializer = if self.match_token(TokenKind::Equal) {
                Some(self.expression()?)
            } else {
                None
            };
            
            self.expect(TokenKind::Semicolon, "expected ';' after variable declaration")?;
            
            return Some(Stmt::VarDecl(super::VariableDecl {
                type_spec,
                name,
                name_span,
                array_dims,
                initializer,
                is_mutable,
                is_static,
                is_extern: false,
            }));
        }
        
        let type_spec = self.parse_type()?;
        let (name, name_span) = self.expect_identifier_with_span("expected variable name")?;
        let array_dims = self.parse_array_dimensions();

        let initializer = if self.match_token(TokenKind::Equal) {
            Some(self.expression()?)
        } else {
            None
        };

        self.expect(TokenKind::Semicolon, "expected ';' after variable declaration")?;

        Some(Stmt::VarDecl(VariableDecl {
            type_spec,
            name,
            name_span,
            array_dims,
            initializer,
            is_mutable,
            is_static,
            is_extern: false,
        }))
    }

    /// Parse if statement
    fn if_statement(&mut self) -> Option<Stmt> {
        self.advance(); // consume 'if'
        
        self.expect(TokenKind::LeftParen, "expected '(' after 'if'")?;
        let condition = self.expression()?;
        self.expect(TokenKind::RightParen, "expected ')' after if condition")?;

        let then_branch = Box::new(self.parse_statement()?);

        let else_branch = if self.match_token(TokenKind::Else) {
            Some(Box::new(self.parse_statement()?))
        } else {
            None
        };

        Some(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    /// Parse while statement
    fn while_statement(&mut self) -> Option<Stmt> {
        self.advance(); // consume 'while'
        
        self.expect(TokenKind::LeftParen, "expected '(' after 'while'")?;
        let condition = self.expression()?;
        self.expect(TokenKind::RightParen, "expected ')' after while condition")?;

        let body = Box::new(self.parse_statement()?);

        Some(Stmt::While { condition, body })
    }

    /// Parse do-while statement
    fn do_while_statement(&mut self) -> Option<Stmt> {
        self.advance(); // consume 'do'
        
        let body = Box::new(self.parse_statement()?);
        
        self.expect(TokenKind::While, "expected 'while' after do body")?;
        self.expect(TokenKind::LeftParen, "expected '(' after 'while'")?;
        let condition = self.expression()?;
        self.expect(TokenKind::RightParen, "expected ')' after condition")?;
        self.expect(TokenKind::Semicolon, "expected ';' after do-while")?;

        Some(Stmt::DoWhile { body, condition })
    }

    /// Parse for statement
    fn for_statement(&mut self) -> Option<Stmt> {
        self.advance(); // consume 'for'
        
        self.expect(TokenKind::LeftParen, "expected '(' after 'for'")?;

        // Initializer
        let init = if self.check(TokenKind::Semicolon) {
            self.advance();
            None
        } else if self.is_declaration_start() {
            Some(Box::new(self.local_var_decl()?))
        } else {
            let expr = self.expression()?;
            self.expect(TokenKind::Semicolon, "expected ';' after for initializer")?;
            Some(Box::new(Stmt::Expression(expr)))
        };

        // Condition
        let condition = if self.check(TokenKind::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };
        self.expect(TokenKind::Semicolon, "expected ';' after for condition")?;

        // Update
        let update = if self.check(TokenKind::RightParen) {
            None
        } else {
            Some(self.expression()?)
        };
        self.expect(TokenKind::RightParen, "expected ')' after for clauses")?;

        let body = Box::new(self.parse_statement()?);

        Some(Stmt::For {
            init,
            condition,
            update,
            body,
        })
    }

    /// Parse switch statement
    fn switch_statement(&mut self) -> Option<Stmt> {
        self.advance(); // consume 'switch'
        
        self.expect(TokenKind::LeftParen, "expected '(' after 'switch'")?;
        let expr = self.expression()?;
        self.expect(TokenKind::RightParen, "expected ')' after switch expression")?;
        
        self.expect(TokenKind::LeftBrace, "expected '{' for switch body")?;

        let mut cases = Vec::new();
        let mut default = None;

        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            if self.match_token(TokenKind::Case) {
                let value = self.expression()?;
                self.expect(TokenKind::Colon, "expected ':' after case value")?;

                let mut body = Vec::new();
                while !self.check(TokenKind::Case) 
                    && !self.check(TokenKind::Default) 
                    && !self.check(TokenKind::RightBrace) 
                    && !self.is_at_end() 
                {
                    if let Some(stmt) = self.parse_statement() {
                        body.push(stmt);
                    }
                }

                cases.push(SwitchCase { value, body });
            } else if self.match_token(TokenKind::Default) {
                self.expect(TokenKind::Colon, "expected ':' after 'default'")?;

                let mut stmts = Vec::new();
                while !self.check(TokenKind::Case) 
                    && !self.check(TokenKind::RightBrace) 
                    && !self.is_at_end() 
                {
                    if let Some(stmt) = self.parse_statement() {
                        stmts.push(stmt);
                    }
                }

                default = Some(Block { statements: stmts });
            } else {
                self.error_at_current(codes::EXPECTED_TOKEN, "expected 'case' or 'default' in switch");
                break;
            }
        }

        self.expect(TokenKind::RightBrace, "expected '}' after switch body")?;

        Some(Stmt::Switch {
            expr,
            cases,
            default,
        })
    }

    /// Parse match statement (CScript-specific for Option)
    fn match_statement(&mut self) -> Option<Stmt> {
        self.advance(); // consume 'match'
        
        self.expect(TokenKind::LeftParen, "expected '(' after 'match'")?;
        let expr = self.expression()?;
        self.expect(TokenKind::RightParen, "expected ')' after match expression")?;
        
        self.expect(TokenKind::LeftBrace, "expected '{' for match body")?;

        let mut arms = Vec::new();

        while !self.check(TokenKind::RightBrace) && !self.is_at_end() {
            let pattern = if self.match_token(TokenKind::Some) {
                self.expect(TokenKind::LeftParen, "expected '(' after 'some'")?;
                let name = self.expect_identifier("expected variable name in some pattern")?;
                self.expect(TokenKind::RightParen, "expected ')' after some pattern")?;
                MatchPattern::Some(name)
            } else if self.match_token(TokenKind::None) {
                MatchPattern::None
            } else if self.match_token(TokenKind::Ok) {
                self.expect(TokenKind::LeftParen, "expected '(' after 'ok'")?;
                let name = self.expect_identifier("expected variable name in ok pattern")?;
                self.expect(TokenKind::RightParen, "expected ')' after ok pattern")?;
                MatchPattern::Ok(name)
            } else if self.match_token(TokenKind::Err) {
                self.expect(TokenKind::LeftParen, "expected '(' after 'err'")?;
                let name = self.expect_identifier("expected variable name in err pattern")?;
                self.expect(TokenKind::RightParen, "expected ')' after err pattern")?;
                MatchPattern::Err(name)
            } else {
                self.error_at_current(codes::EXPECTED_TOKEN, "expected 'some', 'none', 'ok', or 'err' in match arm");
                return None;
            };

            self.expect(TokenKind::FatArrow, "expected '=>' after match pattern")?;
            
            let body = self.block()?;

            arms.push(MatchArm { pattern, body });
        }

        self.expect(TokenKind::RightBrace, "expected '}' after match body")?;

        Some(Stmt::Match { expr, arms })
    }

    /// Parse return statement
    fn return_statement(&mut self) -> Option<Stmt> {
        self.advance(); // consume 'return'
        
        let value = if self.check(TokenKind::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };
        
        self.expect(TokenKind::Semicolon, "expected ';' after return")?;
        
        Some(Stmt::Return(value))
    }

    /// Parse break statement
    fn break_statement(&mut self) -> Option<Stmt> {
        self.advance(); // consume 'break'
        self.expect(TokenKind::Semicolon, "expected ';' after 'break'")?;
        Some(Stmt::Break)
    }

    /// Parse continue statement
    fn continue_statement(&mut self) -> Option<Stmt> {
        self.advance(); // consume 'continue'
        self.expect(TokenKind::Semicolon, "expected ';' after 'continue'")?;
        Some(Stmt::Continue)
    }

    /// Parse goto statement
    fn goto_statement(&mut self) -> Option<Stmt> {
        self.advance(); // consume 'goto'
        let label = self.expect_identifier("expected label after 'goto'")?;
        self.expect(TokenKind::Semicolon, "expected ';' after goto")?;
        Some(Stmt::Goto(label))
    }

    /// Parse expression statement
    fn expression_statement(&mut self) -> Option<Stmt> {
        let expr = self.expression()?;
        self.expect(TokenKind::Semicolon, "expected ';' after expression")?;
        Some(Stmt::Expression(expr))
    }
}
