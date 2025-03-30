pub type Program = Vec<Statement>;
pub struct Identifier(pub String);

pub enum Statement {
    Comment(Identifier),
    Let(Identifier, Option<TypeAnnotation>, Expression),
    Return(Expression),
    Expression(Expression),
    Type(TypeDefinition),
}

pub enum TypeAnnotation {
    Int,
    Float,
    String,
    Bool,
    Unit,
    // alias, record, union
    Named(Identifier),
    // list * int ; option * string
    Product(Identifier, Vec<TypeAnnotation>),
    // fun : int * bool -> String
    Fun(Vec<TypeAnnotation>, Box<TypeAnnotation>),
    // tuple : int * bool * string
    Tuple(Vec<TypeAnnotation>),
}

pub enum TypeDefinition {
    Alias {
        name: Identifier,
        // parameters: Vec<Identifier>,
        target: TypeAnnotation,
    },
    Record {
        name: Identifier,
        parameters: Vec<Identifier>,
        fields: Vec<(Identifier, TypeAnnotation)>,
    },
    Union {
        name: Identifier,
        parameters: Vec<Identifier>,
        variants: Vec<UnionVariant>,
    },
}

pub struct UnionVariant {
    tag: Identifier,
    types: Vec<TypeAnnotation>,
}

pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Unit,
}

pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),
    If {
        cond: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
    },
    Apply {
        func: Box<Expression>,
        argument: Box<Expression>,
    },
    // List.map (fun x->x*x) [1; 2; 3]
    Fun {
        param: Vec<Identifier>,
        body: Program,
    },
    Tuple(Vec<Expression>),
    List(Vec<Expression>),
    // NOTE: Could be a struct
    Record(Identifier, Vec<(Identifier, Expression)>),
    // NOTE: Could be a struct
    // E.g Some 5 ; Ok "Value"
    Variant(Identifier, Identifier, Vec<Expression>),
    MemberAccess(Box<Expression>, Identifier),
}

pub enum Prefix {
    Tilde,
    Bang,
    Minus,
}

pub enum Infix {
    Plus,
    Minus,
    Asterisk,
    ForwardSlash,
    Caret,
    Percent,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    ColonColon,
    PeriodPeriod,
    EqualEqual,
    PlusPlus,
}
