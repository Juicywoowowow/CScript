//! Token definitions for CScript lexer.

use std::fmt;

/// A token with its kind, lexeme, and position
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub offset: usize,
    pub length: usize,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: impl Into<String>, offset: usize, length: usize) -> Self {
        Self {
            kind,
            lexeme: lexeme.into(),
            offset,
            length,
        }
    }

    pub fn eof(offset: usize) -> Self {
        Self::new(TokenKind::Eof, "", offset, 0)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}('{}')", self.kind, self.lexeme)
    }
}

/// All token kinds in CScript
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Literals
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    CharLiteral,
    
    // Identifiers and keywords
    Identifier,
    
    // Type keywords
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    Float32,
    Float64,
    Bool,
    Char,
    Void,
    
    // C-compatible type aliases
    Int,        // alias for int32
    Long,       // alias for int64
    Short,      // alias for int16
    Unsigned,   // modifier
    Signed,     // modifier
    Float,      // alias for float32
    Double,     // alias for float64
    
    // Keywords
    If,
    Else,
    While,
    For,
    Do,
    Return,
    Break,
    Continue,
    Switch,
    Case,
    Default,
    Goto,
    Sizeof,
    
    // CScript-specific keywords
    Mut,        // mutable variable
    Option,     // nullable wrapper
    None,       // null value for Option
    Some,       // unwrap Option
    Match,      // pattern matching
    True,
    False,
    
    // Storage/type modifiers
    Struct,
    Enum,
    Union,
    Typedef,
    Static,
    Extern,
    Volatile,
    Inline,
    
    // Operators
    Plus,           // +
    Minus,          // -
    Star,           // *
    Slash,          // /
    Percent,        // %
    Ampersand,      // &
    Pipe,           // |
    Caret,          // ^
    Tilde,          // ~
    Bang,           // !
    Less,           // <
    Greater,        // >
    Equal,          // =
    Question,       // ?
    Colon,          // :
    Dot,            // .
    Comma,          // ,
    Semicolon,      // ;
    
    // Compound operators
    PlusEqual,      // +=
    MinusEqual,     // -=
    StarEqual,      // *=
    SlashEqual,     // /=
    PercentEqual,   // %=
    AmpersandEqual, // &=
    PipeEqual,      // |=
    CaretEqual,     // ^=
    LessLess,       // <<
    GreaterGreater, // >>
    LessLessEqual,  // <<=
    GreaterGreaterEqual, // >>=
    EqualEqual,     // ==
    BangEqual,      // !=
    LessEqual,      // <=
    GreaterEqual,   // >=
    AmpersandAmpersand, // &&
    PipePipe,       // ||
    PlusPlus,       // ++
    MinusMinus,     // --
    Arrow,          // ->
    FatArrow,       // =>
    ColonColon,     // ::
    
    // Delimiters
    LeftParen,      // (
    RightParen,     // )
    LeftBrace,      // {
    RightBrace,     // }
    LeftBracket,    // [
    RightBracket,   // ]
    
    // Preprocessor (pass-through to C)
    Preprocessor,   // #include, #define, etc.
    
    // Special
    Eof,
    Error,
}

impl TokenKind {
    /// Check if this token is a type keyword
    pub fn is_type_keyword(&self) -> bool {
        matches!(self,
            TokenKind::Int8 | TokenKind::Int16 | TokenKind::Int32 | TokenKind::Int64 |
            TokenKind::Uint8 | TokenKind::Uint16 | TokenKind::Uint32 | TokenKind::Uint64 |
            TokenKind::Float32 | TokenKind::Float64 |
            TokenKind::Bool | TokenKind::Char | TokenKind::Void |
            TokenKind::Int | TokenKind::Long | TokenKind::Short |
            TokenKind::Unsigned | TokenKind::Signed |
            TokenKind::Float | TokenKind::Double |
            TokenKind::Struct | TokenKind::Enum | TokenKind::Union
        )
    }

    /// Check if this token could start a declaration
    pub fn can_start_declaration(&self) -> bool {
        self.is_type_keyword() || matches!(self, 
            TokenKind::Mut | TokenKind::Static | TokenKind::Extern | 
            TokenKind::Typedef | TokenKind::Option | TokenKind::Volatile |
            TokenKind::Inline
        )
    }  

    /// Check if this is an assignment operator
    pub fn is_assignment(&self) -> bool {
        matches!(self,
            TokenKind::Equal | TokenKind::PlusEqual | TokenKind::MinusEqual |
            TokenKind::StarEqual | TokenKind::SlashEqual | TokenKind::PercentEqual |
            TokenKind::AmpersandEqual | TokenKind::PipeEqual | TokenKind::CaretEqual |
            TokenKind::LessLessEqual | TokenKind::GreaterGreaterEqual
        )
    }
}

/// Map string to keyword token kind
pub fn lookup_keyword(ident: &str) -> Option<TokenKind> {
    match ident {
        // CScript types
        "int8" => Some(TokenKind::Int8),
        "int16" => Some(TokenKind::Int16),
        "int32" => Some(TokenKind::Int32),
        "int64" => Some(TokenKind::Int64),
        "uint8" => Some(TokenKind::Uint8),
        "uint16" => Some(TokenKind::Uint16),
        "uint32" => Some(TokenKind::Uint32),
        "uint64" => Some(TokenKind::Uint64),
        "float32" => Some(TokenKind::Float32),
        "float64" => Some(TokenKind::Float64),
        "bool" => Some(TokenKind::Bool),
        
        // C-compatible types
        "int" => Some(TokenKind::Int),
        "long" => Some(TokenKind::Long),
        "short" => Some(TokenKind::Short),
        "unsigned" => Some(TokenKind::Unsigned),
        "signed" => Some(TokenKind::Signed),
        "float" => Some(TokenKind::Float),
        "double" => Some(TokenKind::Double),
        "char" => Some(TokenKind::Char),
        "void" => Some(TokenKind::Void),
        
        // Control flow
        "if" => Some(TokenKind::If),
        "else" => Some(TokenKind::Else),
        "while" => Some(TokenKind::While),
        "for" => Some(TokenKind::For),
        "do" => Some(TokenKind::Do),
        "return" => Some(TokenKind::Return),
        "break" => Some(TokenKind::Break),
        "continue" => Some(TokenKind::Continue),
        "switch" => Some(TokenKind::Switch),
        "case" => Some(TokenKind::Case),
        "default" => Some(TokenKind::Default),
        "goto" => Some(TokenKind::Goto),
        "sizeof" => Some(TokenKind::Sizeof),
        
        // CScript keywords
        "mut" => Some(TokenKind::Mut),
        "Option" => Some(TokenKind::Option),
        "none" => Some(TokenKind::None),
        "some" => Some(TokenKind::Some),
        "match" => Some(TokenKind::Match),
        "true" => Some(TokenKind::True),
        "false" => Some(TokenKind::False),
        
        // Type definitions
        "struct" => Some(TokenKind::Struct),
        "enum" => Some(TokenKind::Enum),
        "union" => Some(TokenKind::Union),
        "typedef" => Some(TokenKind::Typedef),
        
        // Modifiers
        "static" => Some(TokenKind::Static),
        "extern" => Some(TokenKind::Extern),
        "volatile" => Some(TokenKind::Volatile),
        "inline" => Some(TokenKind::Inline),
        
        _ => None,
    }
}
