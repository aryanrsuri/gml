pub type Program = Vec<Statement>;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier(pub String);

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let {
        name: Identifier,
        annotation: Option<TypeAnnotation>,
        value: Expression,
    },
    Return(Expression),
    Expression(Expression),
    Type(TypeDefinition),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation {
    Int,
    Float,
    String,
    Bool,
    Unit,
    List,
    // alias, record, union
    Named(Identifier),
    // list * int ; option * string
    // FIXME: Do I actually need to use a TypeAnnotation for List, or can it just be an identifier?
    Product(Box<TypeAnnotation>, Box<TypeAnnotation>),
    // fun : int * bool -> String
    Fun(Vec<TypeAnnotation>, Box<TypeAnnotation>),
    // tuple : int * bool * string
    Tuple(Vec<TypeAnnotation>),
    // Type variable for polymorphism e.g. 'a
    Var(Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeDefinition {
    Alias {
        name: Identifier,
        params: Vec<Identifier>,
        target: TypeAnnotation,
    },
    Record {
        name: Identifier,
        params: Vec<Identifier>,
        fields: Vec<(Identifier, TypeAnnotation)>,
    },
    Union {
        name: Identifier,
        params: Vec<Identifier>,
        variants: Vec<UnionVariant>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnionVariant {
    pub tag: Identifier,
    pub types: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunParam {
    pub name: Identifier,
    pub annotation: Option<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),
    If {
        cond: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Box<Expression>,
    },
    Apply {
        func: Box<Expression>,
        argument: Box<Expression>,
    },
    // List.map (fun x->x*x) [1; 2; 3]
    Fun {
        params: Vec<FunParam>,
        body: Box<Expression>,
    },
    Tuple(Vec<Expression>),
    List(Vec<Expression>),
    Record(Vec<(Identifier, Expression)>),
    // NOTE: Could be a struct
    // E.g Some 5 ; Ok "Value"
    Variant(Identifier, Identifier, Vec<Expression>),
    MemberAccess(Box<Expression>, Identifier),
    Sequence(Box<Expression>, Box<Expression>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Prefix {
    Tilde,
    Bang,
    Minus,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Infix {
    Plus,
    Minus,
    Asterisk,
    AsteriskAsterisk,
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
