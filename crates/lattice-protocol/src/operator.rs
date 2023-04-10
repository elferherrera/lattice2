#[derive(Debug, PartialEq, Eq)]
pub enum Operator {
    Pipe,
    Not,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    RegexMatch,
    NotRegexMatch,
    Plus,
    Minus,
    Multiply,
    Divide,
    In,
    NotIn,
    Modulo,
    FloorDivision,
    And,
    Or,
    Pow,
    StartsWith,
    EndsWith,
    BitOr,
    BitXor,
    BitAnd,
    ShiftLeft,
    ShiftRight,
}
