//! Expression parser for CScript.
//! Implements a Pratt parser for operator precedence.

use super::{AssignOp, BinaryOp, Expr, Parser, SizeofArg, TypeSpec, UnaryOp};
use crate::diagnostics::{codes, Diagnostic};
use crate::lexer::TokenKind;

/// Trait extension for expression parsing
pub trait ExpressionParser {
    fn expression(&mut self) -> Option<Expr>;
}

impl<'a> ExpressionParser for Parser<'a> {
    fn expression(&mut self) -> Option<Expr> {
        self.parse_assignment()
    }
}

impl<'a> Parser<'a> {
    /// Parse assignment expression (lowest precedence)
    pub(crate) fn parse_assignment(&mut self) -> Option<Expr> {
        let expr = self.parse_ternary()?;

        if let Some(op) = self.match_assign_op() {
            let value = self.parse_assignment()?;
            return Some(Expr::Assign {
                target: Box::new(expr),
                op,
                value: Box::new(value),
            });
        }

        Some(expr)
    }

    fn match_assign_op(&mut self) -> Option<AssignOp> {
        let op = match self.peek().kind {
            TokenKind::Equal => AssignOp::Assign,
            TokenKind::PlusEqual => AssignOp::AddAssign,
            TokenKind::MinusEqual => AssignOp::SubAssign,
            TokenKind::StarEqual => AssignOp::MulAssign,
            TokenKind::SlashEqual => AssignOp::DivAssign,
            TokenKind::PercentEqual => AssignOp::ModAssign,
            TokenKind::AmpersandEqual => AssignOp::BitAndAssign,
            TokenKind::PipeEqual => AssignOp::BitOrAssign,
            TokenKind::CaretEqual => AssignOp::BitXorAssign,
            TokenKind::LessLessEqual => AssignOp::ShlAssign,
            TokenKind::GreaterGreaterEqual => AssignOp::ShrAssign,
            _ => return None,
        };
        self.advance();
        Some(op)
    }

    /// Parse ternary conditional: a ? b : c
    fn parse_ternary(&mut self) -> Option<Expr> {
        let mut expr = self.parse_or()?;

        if self.match_token(TokenKind::Question) {
            let then_expr = self.expression()?;
            self.expect(TokenKind::Colon, "expected ':' in ternary expression")?;
            let else_expr = self.parse_ternary()?;
            
            expr = Expr::Ternary {
                condition: Box::new(expr),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            };
        }

        Some(expr)
    }

    /// Parse logical OR: a || b
    fn parse_or(&mut self) -> Option<Expr> {
        let mut expr = self.parse_and()?;

        while self.match_token(TokenKind::PipePipe) {
            let right = self.parse_and()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::Or,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    /// Parse logical AND: a && b
    fn parse_and(&mut self) -> Option<Expr> {
        let mut expr = self.parse_bitor()?;

        while self.match_token(TokenKind::AmpersandAmpersand) {
            let right = self.parse_bitor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::And,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    /// Parse bitwise OR: a | b
    fn parse_bitor(&mut self) -> Option<Expr> {
        let mut expr = self.parse_bitxor()?;

        while self.match_token(TokenKind::Pipe) {
            let right = self.parse_bitxor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::BitOr,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    /// Parse bitwise XOR: a ^ b
    fn parse_bitxor(&mut self) -> Option<Expr> {
        let mut expr = self.parse_bitand()?;

        while self.match_token(TokenKind::Caret) {
            let right = self.parse_bitand()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::BitXor,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    /// Parse bitwise AND: a & b
    fn parse_bitand(&mut self) -> Option<Expr> {
        let mut expr = self.parse_equality()?;

        while self.match_token(TokenKind::Ampersand) {
            let right = self.parse_equality()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinaryOp::BitAnd,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    /// Parse equality: a == b, a != b
    fn parse_equality(&mut self) -> Option<Expr> {
        let mut expr = self.parse_comparison()?;

        loop {
            let op = match self.peek().kind {
                TokenKind::EqualEqual => BinaryOp::Eq,
                TokenKind::BangEqual => BinaryOp::Ne,
                _ => break,
            };
            self.advance();
            let right = self.parse_comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    /// Parse comparison: a < b, a <= b, a > b, a >= b
    fn parse_comparison(&mut self) -> Option<Expr> {
        let mut expr = self.parse_shift()?;

        loop {
            let op = match self.peek().kind {
                TokenKind::Less => BinaryOp::Lt,
                TokenKind::LessEqual => BinaryOp::Le,
                TokenKind::Greater => BinaryOp::Gt,
                TokenKind::GreaterEqual => BinaryOp::Ge,
                _ => break,
            };
            self.advance();
            let right = self.parse_shift()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    /// Parse shift: a << b, a >> b
    fn parse_shift(&mut self) -> Option<Expr> {
        let mut expr = self.parse_additive()?;

        loop {
            let op = match self.peek().kind {
                TokenKind::LessLess => BinaryOp::Shl,
                TokenKind::GreaterGreater => BinaryOp::Shr,
                _ => break,
            };
            self.advance();
            let right = self.parse_additive()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    /// Parse additive: a + b, a - b
    fn parse_additive(&mut self) -> Option<Expr> {
        let mut expr = self.parse_multiplicative()?;

        loop {
            let op = match self.peek().kind {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    /// Parse multiplicative: a * b, a / b, a % b
    fn parse_multiplicative(&mut self) -> Option<Expr> {
        let mut expr = self.parse_unary()?;

        loop {
            let op = match self.peek().kind {
                TokenKind::Star => BinaryOp::Mul,
                TokenKind::Slash => BinaryOp::Div,
                TokenKind::Percent => BinaryOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }

        Some(expr)
    }

    /// Parse unary: -a, !a, ~a, *a, &a, ++a, --a
    fn parse_unary(&mut self) -> Option<Expr> {
        let op = match self.peek().kind {
            TokenKind::Minus => Some(UnaryOp::Neg),
            TokenKind::Bang => Some(UnaryOp::Not),
            TokenKind::Tilde => Some(UnaryOp::BitNot),
            TokenKind::Star => Some(UnaryOp::Deref),
            TokenKind::Ampersand => Some(UnaryOp::AddrOf),
            TokenKind::PlusPlus => Some(UnaryOp::PreInc),
            TokenKind::MinusMinus => Some(UnaryOp::PreDec),
            _ => None,
        };

        if let Some(op) = op {
            self.advance();
            let operand = self.parse_unary()?;
            return Some(Expr::Unary {
                op,
                operand: Box::new(operand),
            });
        }

        // Cast expression: (type)expr
        if self.check(TokenKind::LeftParen) {
            if let Some(cast) = self.try_parse_cast() {
                return Some(cast);
            }
        }

        // sizeof
        if self.match_token(TokenKind::Sizeof) {
            return self.parse_sizeof();
        }

        self.parse_postfix()
    }

    /// Try to parse a cast expression
    fn try_parse_cast(&mut self) -> Option<Expr> {
        // Save position for backtracking
        let saved_pos = self.current;

        self.advance(); // consume '('

        // Try to parse a type
        if let Some(type_spec) = self.parse_type() {
            if self.match_token(TokenKind::RightParen) {
                // It's a cast if followed by a unary expression
                if let Some(expr) = self.parse_unary() {
                    return Some(Expr::Cast {
                        type_spec,
                        expr: Box::new(expr),
                    });
                }
            }
        }

        // Not a cast, backtrack
        self.current = saved_pos;
        None
    }

    /// Parse sizeof expression
    fn parse_sizeof(&mut self) -> Option<Expr> {
        if self.match_token(TokenKind::LeftParen) {
            // Could be sizeof(type) or sizeof(expr)
            let saved_pos = self.current;

            if let Some(type_spec) = self.parse_type() {
                if self.match_token(TokenKind::RightParen) {
                    return Some(Expr::Sizeof(SizeofArg::Type(type_spec)));
                }
            }

            // Backtrack and parse as expression
            self.current = saved_pos;
            let expr = self.expression()?;
            self.expect(TokenKind::RightParen, "expected ')' after sizeof expression")?;
            Some(Expr::Sizeof(SizeofArg::Expr(Box::new(expr))))
        } else {
            // sizeof expr (without parens)
            let expr = self.parse_unary()?;
            Some(Expr::Sizeof(SizeofArg::Expr(Box::new(expr))))
        }
    }

    /// Parse postfix: a++, a--, a[i], a.b, a->b, a(args)
    fn parse_postfix(&mut self) -> Option<Expr> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.match_token(TokenKind::LeftParen) {
                // Function call
                let args = self.parse_call_args()?;
                expr = Expr::Call {
                    callee: Box::new(expr),
                    args,
                };
            } else if self.match_token(TokenKind::LeftBracket) {
                // Array index
                let index = self.expression()?;
                self.expect(TokenKind::RightBracket, "expected ']' after array index")?;
                expr = Expr::Index {
                    array: Box::new(expr),
                    index: Box::new(index),
                };
            } else if self.match_token(TokenKind::Dot) {
                // Member access
                let member = self.expect_identifier("expected member name after '.'")?;
                
                // Check for .unwrap() or .unwrap_or()
                if member == "unwrap" && self.check(TokenKind::LeftParen) {
                    self.advance(); // consume '('
                    self.expect(TokenKind::RightParen, "expected ')' after unwrap")?;
                    expr = Expr::Unwrap(Box::new(expr));
                } else if member == "unwrap_or" && self.check(TokenKind::LeftParen) {
                    self.advance(); // consume '('
                    let default = self.expression()?;
                    self.expect(TokenKind::RightParen, "expected ')' after unwrap_or")?;
                    expr = Expr::UnwrapOr {
                        expr: Box::new(expr),
                        default: Box::new(default),
                    };
                } else {
                    expr = Expr::Member {
                        object: Box::new(expr),
                        member,
                    };
                }
            } else if self.match_token(TokenKind::Arrow) {
                // Pointer member access
                let member = self.expect_identifier("expected member name after '->'")?;
                expr = Expr::PtrMember {
                    pointer: Box::new(expr),
                    member,
                };
            } else if self.match_token(TokenKind::PlusPlus) {
                // Post-increment
                expr = Expr::Unary {
                    op: UnaryOp::PostInc,
                    operand: Box::new(expr),
                };
            } else if self.match_token(TokenKind::MinusMinus) {
                // Post-decrement
                expr = Expr::Unary {
                    op: UnaryOp::PostDec,
                    operand: Box::new(expr),
                };
            } else {
                break;
            }
        }

        Some(expr)
    }

    /// Parse function call arguments
    fn parse_call_args(&mut self) -> Option<Vec<Expr>> {
        let mut args = Vec::new();

        if !self.check(TokenKind::RightParen) {
            loop {
                args.push(self.expression()?);
                if !self.match_token(TokenKind::Comma) {
                    break;
                }
            }
        }

        self.expect(TokenKind::RightParen, "expected ')' after function arguments")?;
        Some(args)
    }

    /// Parse primary expressions: literals, identifiers, parenthesized
    fn parse_primary(&mut self) -> Option<Expr> {
        let token = self.peek().clone();

        match token.kind {
            TokenKind::IntLiteral => {
                self.advance();
                let value = parse_int_literal(&token.lexeme);
                Some(Expr::IntLiteral(value))
            }

            TokenKind::FloatLiteral => {
                self.advance();
                let value: f64 = token.lexeme
                    .trim_end_matches(|c| c == 'f' || c == 'F')
                    .parse()
                    .unwrap_or(0.0);
                Some(Expr::FloatLiteral(value))
            }

            TokenKind::StringLiteral => {
                self.advance();
                // Remove quotes and handle escapes
                let s = &token.lexeme[1..token.lexeme.len() - 1];
                Some(Expr::StringLiteral(unescape_string(s)))
            }

            TokenKind::CharLiteral => {
                self.advance();
                let s = &token.lexeme[1..token.lexeme.len() - 1];
                let c = unescape_char(s);
                Some(Expr::CharLiteral(c))
            }

            TokenKind::True => {
                self.advance();
                Some(Expr::BoolLiteral(true))
            }

            TokenKind::False => {
                self.advance();
                Some(Expr::BoolLiteral(false))
            }

            TokenKind::None => {
                self.advance();
                Some(Expr::None)
            }

            TokenKind::Identifier => {
                self.advance();
                Some(Expr::Identifier(token.lexeme.clone()))
            }

            TokenKind::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.expect(TokenKind::RightParen, "expected ')' after expression")?;
                Some(expr)
            }

            TokenKind::LeftBrace => {
                self.parse_initializer_list()
            }

            _ => {
                self.error_at_current(
                    codes::EXPECTED_EXPRESSION,
                    &format!("expected expression, found '{}'", token.lexeme),
                );
                None
            }
        }
    }

    /// Parse initializer list: { 1, 2, 3 } or { .x = 1, .y = 2 }
    fn parse_initializer_list(&mut self) -> Option<Expr> {
        self.expect(TokenKind::LeftBrace, "expected '{'")?;

        if self.check(TokenKind::RightBrace) {
            self.advance();
            return Some(Expr::InitList(Vec::new()));
        }

        // Check if it's a designated initializer
        if self.check(TokenKind::Dot) {
            let mut fields = Vec::new();
            
            loop {
                self.expect(TokenKind::Dot, "expected '.' for designated initializer")?;
                let name = self.expect_identifier("expected field name")?;
                self.expect(TokenKind::Equal, "expected '=' after field name")?;
                let value = self.expression()?;
                fields.push((name, value));

                if !self.match_token(TokenKind::Comma) {
                    break;
                }
                
                if self.check(TokenKind::RightBrace) {
                    break; // Trailing comma
                }
            }

            self.expect(TokenKind::RightBrace, "expected '}' after initializer list")?;
            return Some(Expr::DesignatedInit(fields));
        }

        // Regular initializer list
        let mut exprs = Vec::new();
        
        loop {
            exprs.push(self.expression()?);
            
            if !self.match_token(TokenKind::Comma) {
                break;
            }
            
            if self.check(TokenKind::RightBrace) {
                break; // Trailing comma
            }
        }

        self.expect(TokenKind::RightBrace, "expected '}' after initializer list")?;
        Some(Expr::InitList(exprs))
    }
}

/// Parse integer literal (handles hex, octal, binary)
fn parse_int_literal(s: &str) -> i64 {
    let s = s.trim_end_matches(|c| matches!(c, 'l' | 'L' | 'u' | 'U'));
    
    if s.starts_with("0x") || s.starts_with("0X") {
        i64::from_str_radix(&s[2..], 16).unwrap_or(0)
    } else if s.starts_with("0b") || s.starts_with("0B") {
        i64::from_str_radix(&s[2..], 2).unwrap_or(0)
    } else if s.starts_with('0') && s.len() > 1 && s.chars().nth(1).map_or(false, |c| c.is_ascii_digit()) {
        i64::from_str_radix(&s[1..], 8).unwrap_or(0)
    } else {
        s.parse().unwrap_or(0)
    }
}

/// Unescape string literal
fn unescape_string(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('r') => result.push('\r'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('\'') => result.push('\''),
                Some('"') => result.push('"'),
                Some('0') => result.push('\0'),
                Some(c) => {
                    result.push('\\');
                    result.push(c);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }

    result
}

/// Unescape char literal
fn unescape_char(s: &str) -> char {
    let mut chars = s.chars();
    match chars.next() {
        Some('\\') => match chars.next() {
            Some('n') => '\n',
            Some('r') => '\r',
            Some('t') => '\t',
            Some('\\') => '\\',
            Some('\'') => '\'',
            Some('0') => '\0',
            Some(c) => c,
            None => '\\',
        },
        Some(c) => c,
        None => '\0',
    }
}
