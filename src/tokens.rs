#[derive(Debug, PartialEq)]
pub enum Token {
    // Bindings and Keywords
    Identifier(String),
    Comment(String),

    // Literals
    Char(char),
    String(String),
    Int(String),
    Float(String),
    Bool(bool),

    // Operators
    Bang,
    Tilde,
    Percent,
    LessThan,
    Colon,
    Period,
    Ampersand,
    Plus,
    Minus,
    Asterisk,
    ForwardSlash,
    Equal,
    GreaterThan,
    PlusPlus,
    Caret,
    VerticalBar,
    NotEqual,
    SingleQuote,
    LeftParen,
    Unit, // ()
    RightParen,
    RightArrow, // ->
    LeftArrow,  // <-
    PeriodPeriod,
    ColonColon,
    EqualEqual,
    Comma,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    QuestionMark,
    SemiColon,
    SemiColonSemiColon,
    // NOTE: Do I need these tokens?
    CommentStart, // (*
    CommentEnd,   // *)

    Illegal,
    EOF,
}
