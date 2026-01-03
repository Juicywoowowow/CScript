//! Lexer module for tokenizing CScript source code.

mod token;
mod scanner;

pub use token::{Token, TokenKind};
pub use scanner::Scanner;
