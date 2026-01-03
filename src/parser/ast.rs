//! Abstract Syntax Tree definitions for CScript.

/// Source span for tracking positions in error messages
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Span {
    /// Byte offset from start of source
    pub offset: usize,
    /// Length in bytes
    pub length: usize,
}

impl Span {
    pub fn new(offset: usize, length: usize) -> Self {
        Self { offset, length }
    }
    
    /// Create a span that covers both self and other
    pub fn merge(&self, other: &Span) -> Span {
        let start = self.offset.min(other.offset);
        let end = (self.offset + self.length).max(other.offset + other.length);
        Span::new(start, end - start)
    }
}

/// The root of the AST - a complete program
#[derive(Debug, Clone)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

/// Top-level declarations
#[derive(Debug, Clone)]
pub enum Declaration {
    Function(FunctionDecl),
    Variable(VariableDecl),
    Struct(StructDecl),
    Enum(EnumDecl),
    Union(UnionDecl),
    Typedef(TypedefDecl),
    Preprocessor(String),
}

/// Function declaration
#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub return_type: TypeSpec,
    pub name: String,
    pub name_span: Span,  // Source location of the function name
    pub params: Vec<Parameter>,
    pub body: Option<Block>,
    pub is_static: bool,
    pub is_extern: bool,
    pub is_inline: bool,
}

/// Function parameter
#[derive(Debug, Clone)]
pub struct Parameter {
    pub type_spec: TypeSpec,
    pub name: Option<String>,
    pub name_span: Option<Span>,  // Source location of parameter name (if named)
    pub is_mutable: bool,
}

/// Variable declaration
#[derive(Debug, Clone)]
pub struct VariableDecl {
    pub type_spec: TypeSpec,
    pub name: String,
    pub name_span: Span,  // Source location of the variable name
    pub array_dims: Vec<Option<Expr>>,
    pub initializer: Option<Expr>,
    pub is_mutable: bool,
    pub is_static: bool,
    pub is_extern: bool,
}

/// Struct declaration
#[derive(Debug, Clone)]
pub struct StructDecl {
    pub name: Option<String>,
    pub fields: Vec<StructField>,
}

/// Struct field
#[derive(Debug, Clone)]
pub struct StructField {
    pub type_spec: TypeSpec,
    pub name: String,
    pub array_dims: Vec<Option<Expr>>,
    pub is_mutable: bool,
}

/// Enum declaration
#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub name: Option<String>,
    pub variants: Vec<EnumVariant>,
}

/// Enum variant
#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: String,
    pub value: Option<Expr>,
}

/// Union declaration
#[derive(Debug, Clone)]
pub struct UnionDecl {
    pub name: Option<String>,
    pub fields: Vec<UnionField>,
}

/// Union field
#[derive(Debug, Clone)]
pub struct UnionField {
    pub type_spec: TypeSpec,
    pub name: String,
}

/// Typedef declaration
#[derive(Debug, Clone)]
pub struct TypedefDecl {
    pub original_type: TypeSpec,
    pub new_name: String,
}

/// Type specifier with modifiers
#[derive(Debug, Clone)]
pub struct TypeSpec {
    pub base: BaseType,
    pub pointer_depth: usize,
    pub is_const: bool,
    pub is_volatile: bool,
}

impl TypeSpec {
    pub fn is_pointer(&self) -> bool {
        self.pointer_depth > 0
    }

    pub fn is_option(&self) -> bool {
        matches!(self.base, BaseType::Option(_))
    }
    
    pub fn is_result(&self) -> bool {
        matches!(self.base, BaseType::Result(_, _))
    }
}

/// Base types
#[derive(Debug, Clone)]
pub enum BaseType {
    // CScript integer types
    Int8,
    Int16,
    Int32,
    Int64,
    Uint8,
    Uint16,
    Uint32,
    Uint64,
    
    // CScript float types
    Float32,
    Float64,
    
    // C-compatible types
    Int,
    Long,
    Short,
    Float,
    Double,
    Char,
    Void,
    Bool,
    
    // Unsigned variants
    UnsignedInt,
    UnsignedLong,
    UnsignedShort,
    UnsignedChar,
    
    // Compound types
    Struct(String),
    Enum(String),
    Union(String),
    Named(String),  // typedef'd name
    
    // CScript-specific
    Option(Box<TypeSpec>),
    Result(Box<TypeSpec>, Box<TypeSpec>),  // Result<T, E>
}

impl BaseType {
    /// Get the C equivalent type name
    pub fn to_c_type(&self) -> String {
        match self {
            BaseType::Int8 => "int8_t".to_string(),
            BaseType::Int16 => "int16_t".to_string(),
            BaseType::Int32 => "int32_t".to_string(),
            BaseType::Int64 => "int64_t".to_string(),
            BaseType::Uint8 => "uint8_t".to_string(),
            BaseType::Uint16 => "uint16_t".to_string(),
            BaseType::Uint32 => "uint32_t".to_string(),
            BaseType::Uint64 => "uint64_t".to_string(),
            BaseType::Float32 => "float".to_string(),
            BaseType::Float64 => "double".to_string(),
            BaseType::Int => "int".to_string(),
            BaseType::Long => "long".to_string(),
            BaseType::Short => "short".to_string(),
            BaseType::Float => "float".to_string(),
            BaseType::Double => "double".to_string(),
            BaseType::Char => "char".to_string(),
            BaseType::Void => "void".to_string(),
            BaseType::Bool => "_Bool".to_string(),
            BaseType::UnsignedInt => "unsigned int".to_string(),
            BaseType::UnsignedLong => "unsigned long".to_string(),
            BaseType::UnsignedShort => "unsigned short".to_string(),
            BaseType::UnsignedChar => "unsigned char".to_string(),
            BaseType::Struct(name) => format!("struct {}", name),
            BaseType::Enum(name) => format!("enum {}", name),
            BaseType::Union(name) => format!("union {}", name),
            BaseType::Named(name) => name.clone(),
            BaseType::Option(inner) => {
                // Option types become a struct with is_some flag
                format!("Option_{}", inner.base.to_c_type().replace(' ', "_"))
            }
            BaseType::Result(ok_type, err_type) => {
                // Result types become a struct with is_ok flag and union
                format!(
                    "Result_{}_{}",
                    ok_type.base.to_c_type().replace(' ', "_").replace('*', "ptr"),
                    err_type.base.to_c_type().replace(' ', "_").replace('*', "ptr")
                )
            }
        }
    }
}

/// A block of statements
#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

/// Statements
#[derive(Debug, Clone)]
pub enum Stmt {
    /// Expression statement
    Expression(Expr),
    
    /// Variable declaration (local)
    VarDecl(VariableDecl),
    
    /// Block statement
    Block(Block),
    
    /// If statement
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    
    /// While loop
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    
    /// Do-while loop
    DoWhile {
        body: Box<Stmt>,
        condition: Expr,
    },
    
    /// For loop
    For {
        init: Option<Box<Stmt>>,
        condition: Option<Expr>,
        update: Option<Expr>,
        body: Box<Stmt>,
    },
    
    /// Switch statement
    Switch {
        expr: Expr,
        cases: Vec<SwitchCase>,
        default: Option<Block>,
    },
    
    /// Match expression (CScript)
    Match {
        expr: Expr,
        arms: Vec<MatchArm>,
    },
    
    /// Return statement
    Return(Option<Expr>),
    
    /// Break statement
    Break,
    
    /// Continue statement
    Continue,
    
    /// Goto statement
    Goto(String),
    
    /// Label statement
    Label(String),
    
    /// Local struct declaration
    StructDecl(StructDecl),
    
    /// Empty statement
    Empty,
}

/// Switch case
#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub value: Expr,
    pub body: Vec<Stmt>,
}

/// Match arm for Option pattern matching
#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: MatchPattern,
    pub body: Block,
}

/// Match patterns
#[derive(Debug, Clone)]
pub enum MatchPattern {
    Some(String),  // some(name) => binds value to name
    None,          // none =>
    Ok(String),    // ok(name) => binds ok value to name (for Result)
    Err(String),   // err(name) => binds error value to name (for Result)
}

/// Expressions
#[derive(Debug, Clone)]
pub enum Expr {
    /// Integer literal
    IntLiteral(i64),
    
    /// Float literal
    FloatLiteral(f64),
    
    /// String literal
    StringLiteral(String),
    
    /// Char literal
    CharLiteral(char),
    
    /// Boolean literal
    BoolLiteral(bool),
    
    /// None literal (for Option)
    None,
    
    /// Identifier with span
    Identifier(String),
    
    /// Binary operation
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
        span: Span,
    },
    
    /// Unary operation
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    
    /// Assignment
    Assign {
        target: Box<Expr>,
        op: AssignOp,
        value: Box<Expr>,
        span: Span,
    },
    
    /// Function call with span
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
    
    /// Array/pointer indexing
    Index {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    
    /// Member access (.)
    Member {
        object: Box<Expr>,
        member: String,
    },
    
    /// Pointer member access (->)
    PtrMember {
        pointer: Box<Expr>,
        member: String,
    },
    
    /// Cast expression
    Cast {
        type_spec: TypeSpec,
        expr: Box<Expr>,
    },
    
    /// Sizeof expression
    Sizeof(SizeofArg),
    
    /// Ternary conditional
    Ternary {
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    
    /// Comma expression
    Comma(Vec<Expr>),
    
    /// Initializer list { 1, 2, 3 }
    InitList(Vec<Expr>),
    
    /// Designated initializer { .x = 1, .y = 2 }
    DesignatedInit(Vec<(String, Expr)>),
    
    /// Unwrap Option (.unwrap())
    Unwrap(Box<Expr>),
    
    /// Unwrap with default (.unwrap_or(default))
    UnwrapOr {
        expr: Box<Expr>,
        default: Box<Expr>,
    },
    
    /// Check if Option has value (.is_some())
    IsSome(Box<Expr>),
    
    /// Check if Option is empty (.is_none())
    IsNone(Box<Expr>),
    
    /// Ok constructor for Result - ok(value)
    Ok(Box<Expr>),
    
    /// Err constructor for Result - err(value)
    Err(Box<Expr>),
    
    /// Try operator (?) - propagates errors
    Try {
        expr: Box<Expr>,
        span: Span,
    },
    
    /// Check if Result is ok (.is_ok())
    IsOk(Box<Expr>),
    
    /// Check if Result is err (.is_err())
    IsErr(Box<Expr>),
    
    /// Unwrap error value (.unwrap_err())
    UnwrapErr(Box<Expr>),
}

/// Sizeof argument
#[derive(Debug, Clone)]
pub enum SizeofArg {
    Type(TypeSpec),
    Expr(Box<Expr>),
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

impl BinaryOp {
    pub fn to_c_str(&self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::Shl => "<<",
            BinaryOp::Shr => ">>",
            BinaryOp::Eq => "==",
            BinaryOp::Ne => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Ge => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        }
    }
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,        // -
    Not,        // !
    BitNot,     // ~
    Deref,      // *
    AddrOf,     // &
    PreInc,     // ++x
    PreDec,     // --x
    PostInc,    // x++
    PostDec,    // x--
}

impl UnaryOp {
    pub fn to_c_str(&self) -> &'static str {
        match self {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
            UnaryOp::BitNot => "~",
            UnaryOp::Deref => "*",
            UnaryOp::AddrOf => "&",
            UnaryOp::PreInc => "++",
            UnaryOp::PreDec => "--",
            UnaryOp::PostInc => "++",
            UnaryOp::PostDec => "--",
        }
    }
}

/// Assignment operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOp {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    ShlAssign,
    ShrAssign,
}

impl AssignOp {
    pub fn to_c_str(&self) -> &'static str {
        match self {
            AssignOp::Assign => "=",
            AssignOp::AddAssign => "+=",
            AssignOp::SubAssign => "-=",
            AssignOp::MulAssign => "*=",
            AssignOp::DivAssign => "/=",
            AssignOp::ModAssign => "%=",
            AssignOp::BitAndAssign => "&=",
            AssignOp::BitOrAssign => "|=",
            AssignOp::BitXorAssign => "^=",
            AssignOp::ShlAssign => "<<=",
            AssignOp::ShrAssign => ">>=",
        }
    }
}
