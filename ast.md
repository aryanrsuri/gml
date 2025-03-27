```rust
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Char(char),
    Ident(String),      // Variable or function name
    Application(Box<Expr>, Vec<Expr>), // Function application: f x y
    Let {
        name: String,
        params: Vec<String>, // Function parameters (empty for values)
        ty: Option<Type>,    // Optional type annotation
        value: Box<Expr>,    // Function body or value
    },
    Lambda {
        params: Vec<String>, // Anonymous function parameters
        body: Box<Expr>,     // Function body
    },
    Match {
        expr: Box<Expr>,
        cases: Vec<(Pattern, Expr)>,  // Match cases
    },
    Record(Vec<(String, Expr)>),  // { key: value, key2: value } (runtime record)
    Tuple(Vec<Expr>),             // (expr1, expr2, expr3)
    Operator {
        op: String,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Ident(String),
    Literal(Expr),  // Integer(4), Boolean(true), etc.
    Tuple(Vec<Pattern>),
    Constructor(String, Vec<Pattern>), // Algebraic types: Some x
}

// Type system AST
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Char,
    Unit,
    Tuple(Vec<Type>),
    Record(Vec<(String, Type)>),  // Type-level record definition
    Function(Vec<Type>, Box<Type>), // a -> b -> c (curried functions)
    TypeVar(String), // Polymorphic types 'a, 'b
    Algebraic(String, Vec<Type>), // ADT like Option<'a>
}

// Top-level definitions
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(String, Option<Type>, Expr), // let x : int = 4
    TypeDef(TypeDef), // type definition (alias, record, or ADT)
}

// Type definitions
#[derive(Debug, Clone, PartialEq)]
pub enum TypeDef {
    TypeAlias(String, Type), // type alias: type number = int
    RecordType(String, Vec<(String, Type)>), // record type: type card = { id: string; }
    TaggedUnionType(String, Vec<TypeVariant>), // tagged union: type state = | alive | dead
}

// Algebraic data types (used for tagged unions or sum types)
#[derive(Debug, Clone, PartialEq)]
pub struct TypeVariant {
    pub name: String,
    pub fields: Vec<Type>, // Fields for the variant, empty for simple ones like "alive"
}


```
