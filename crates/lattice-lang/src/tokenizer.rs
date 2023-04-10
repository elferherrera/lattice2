use std::{
    iter::{Enumerate, Peekable},
    slice::Iter,
};

use crate::parse_error::ParseError;
use lattice_protocol::{PathType, Span, StringReplace};

#[derive(Debug, PartialEq)]
pub enum TokenType<'source> {
    // Single character
    Pipe,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    Star,
    Slash,
    Semicolon,
    Colon,
    EOL,
    // One or two characters
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    EqualLike,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    DefType,
    // Keywords
    False,
    True,
    Alias,
    Let,
    And,
    Or,
    If,
    Else,
    For,
    In,
    Def,
    Return,
    While,
    Break,
    Continue,
    // Literals
    Int(i64),
    Float(f64),
    String(&'source [u8]),
    Var(&'source [u8]),
    Comment(&'source [u8]),
    Identifier(&'source [u8]),
    Flag(&'source [u8]),
    Range {
        start: Option<i64>,
        end: Option<i64>,
    },
    CellPath {
        value: Option<&'source [u8]>,
        path: Vec<PathType<'source>>,
    },
    Interpolation {
        string: &'source [u8],
        replacements: Vec<StringReplace<'source>>,
    },
    // Extra
    EOF,
}

impl<'source> TokenType<'source> {
    fn try_char(char: &u8) -> Option<Self> {
        match char {
            b'(' => Some(Self::LeftParen),
            b')' => Some(Self::RightParen),
            b'{' => Some(Self::LeftBrace),
            b'}' => Some(Self::RightBrace),
            b'[' => Some(Self::LeftBracket),
            b']' => Some(Self::RightBracket),
            b',' => Some(Self::Comma),
            b'.' => Some(Self::Dot),
            b'-' => Some(Self::Minus),
            b'+' => Some(Self::Plus),
            b'*' => Some(Self::Star),
            b'/' => Some(Self::Slash),
            b';' => Some(Self::Semicolon),
            b':' => Some(Self::Colon),
            b'\n' => Some(Self::EOL),
            _ => None,
        }
    }

    fn try_keyword(chars: &[u8]) -> Option<Self> {
        match chars {
            b"alias" => Some(TokenType::Alias),
            b"let" => Some(TokenType::Let),
            b"false" => Some(TokenType::False),
            b"true" => Some(TokenType::True),
            b"and" => Some(TokenType::And),
            b"or" => Some(TokenType::Or),
            b"if" => Some(TokenType::If),
            b"else" => Some(TokenType::Else),
            b"for" => Some(TokenType::For),
            b"in" => Some(TokenType::In),
            b"def" => Some(TokenType::Def),
            b"return" => Some(TokenType::Return),
            b"while" => Some(TokenType::While),
            b"break" => Some(TokenType::Break),
            b"continue" => Some(TokenType::Continue),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Token<'source> {
    pub token_type: TokenType<'source>,
    pub span: Span,
}

impl<'source> Token<'source> {
    fn new(token_type: TokenType<'source>, span: Span) -> Self {
        Token { token_type, span }
    }
}

pub struct TokenizeResult<'source> {
    pub tokens: Vec<Token<'source>>,
    pub errors: Option<Vec<ParseError>>,
}

pub fn tokenize(source: &[u8]) -> TokenizeResult {
    let mut iter = source.iter().enumerate().peekable();
    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    while let Some((idx, c)) = iter.next() {
        match c {
            b' ' => continue,
            b'(' | b')' | b'{' | b'}' | b'[' | b']' | b',' | b'+' | b'*' | b'/' | b';' | b':'
            | b'\n' => {
                if let Some(token) = TokenType::try_char(c) {
                    tokens.push(Token::new(token, Span::new(idx, idx + 1)));
                };
            }
            b'|' => {
                let token = Token::new(TokenType::Pipe, Span::new(idx, idx + 1));

                if let Some(prev) = tokens.last_mut() {
                    match prev.token_type {
                        TokenType::EOL | TokenType::Comment(..) => *prev = token,
                        _ => tokens.push(token),
                    }
                } else {
                    tokens.push(token)
                }
            }
            b'=' => {
                let options = vec![(b'=', TokenType::EqualEqual), (b'~', TokenType::EqualLike)];
                let token = process_double_char(TokenType::Equal, idx, &mut iter, options);
                tokens.push(token)
            }
            b'>' => {
                let options = vec![(b'=', TokenType::GreaterEqual)];
                let token = process_double_char(TokenType::Greater, idx, &mut iter, options);
                tokens.push(token)
            }
            b'<' => {
                let options = vec![(b'=', TokenType::LessEqual)];
                let token = process_double_char(TokenType::Less, idx, &mut iter, options);
                tokens.push(token)
            }
            b'!' => {
                let options = vec![(b'=', TokenType::BangEqual)];
                let token = process_double_char(TokenType::Bang, idx, &mut iter, options);
                tokens.push(token)
            }
            b'#' => {
                let idx_end = consume_until(&mut iter, |c| c == &b'\n').unwrap_or(source.len());
                let comment = &source[idx + 1..idx_end];
                let token = Token::new(TokenType::Comment(comment), Span::new(idx, idx_end));
                tokens.push(token)
            }
            b'$' => {
                let token = process_cellpath(idx, source, &mut iter);
                tokens.push(token)
            }
            b'-' => {
                let is_flag = iter
                    .peek()
                    .map(|(_, c)| c.is_ascii_alphabetic() || c == &&b'-')
                    .unwrap_or(false);

                let token = if is_flag {
                    process_flag(idx, source, &mut iter)
                } else {
                    let options = vec![(b'>', TokenType::DefType)];
                    process_double_char(TokenType::Minus, idx, &mut iter, options)
                };

                tokens.push(token)
            }
            b'"' => match process_string(c, idx, source, &mut iter, Some(b'\n')) {
                Ok(token) => tokens.push(token),
                Err(error) => errors.push(error),
            },
            b'`' => match process_string(c, idx, source, &mut iter, None) {
                Ok(token) => tokens.push(token),
                Err(error) => errors.push(error),
            },
            b'f' => {
                let is_interpolation = iter
                    .peek()
                    .map(|(_, c)| c == &&b'"' || c == &&b'`')
                    .unwrap_or(false);

                if is_interpolation {
                    let res = process_interpolation(c, idx, source, &mut iter);
                    match res {
                        Ok(token) => tokens.push(token),
                        Err(error) => errors.push(error),
                    }
                } else {
                    let token = process_item(idx, source, &mut iter);
                    tokens.push(token)
                }
            }
            _ => {
                let token = process_item(idx, source, &mut iter);
                tokens.push(token)
            }
        }
    }

    tokens.push(Token::new(
        TokenType::EOF,
        Span::new(source.len(), source.len()),
    ));

    let errors = (!errors.is_empty()).then_some(errors);
    TokenizeResult { tokens, errors }
}

type SourceIter<'source> = Peekable<Enumerate<Iter<'source, u8>>>;

fn consume_until<F>(iter: &mut SourceIter<'_>, p: F) -> Option<usize>
where
    F: Fn(&u8) -> bool,
{
    while let Some((idx, c)) = iter.peek() {
        if p(c) {
            return Some(*idx);
        }

        iter.next();
    }

    None
}

enum ConsumeStatus {
    Found,
    NotFound,
    Error(ParseError),
}

fn try_consume_until<F>(
    idx: usize,
    iter: &mut SourceIter<'_>,
    error: ParseError,
    f: F,
) -> Result<usize, ParseError>
where
    F: Fn(&u8, Option<&(usize, &u8)>, Span) -> ConsumeStatus,
{
    while let Some((idx_end, c)) = iter.next() {
        let span = Span::new(idx, idx_end);

        match f(c, iter.peek(), span) {
            ConsumeStatus::Found => return Ok(idx_end),
            ConsumeStatus::NotFound => continue,
            ConsumeStatus::Error(e) => return Err(e),
        };
    }

    Err(error)
}

fn get_chars<'source>(
    idx: usize,
    source: &'source [u8],
    iter: &mut SourceIter<'source>,
) -> (&'source [u8], Span) {
    let idx_end = consume_until(iter, |c| {
        !c.is_ascii_alphanumeric() && c != &b'_' && c != &b'.'
    })
    .unwrap_or(source.len());

    let chars = &source[idx..idx_end];
    let span = Span::new(idx, idx_end);

    (chars, span)
}

fn process_interpolation<'source>(
    marker: &u8,
    idx: usize,
    source: &'source [u8],
    iter: &mut SourceIter<'source>,
) -> Result<Token<'source>, ParseError> {
    let mut flag = false;
    let mut inner = 0;
    while let Some((idx_end, c)) = iter.next() {
        if c == marker {
            let chars = &source[idx + 1..idx_end];
            let span = Span::new(idx, idx_end + 1);
            return Ok(Token::new(TokenType::String(chars), span));
        }
        
        if c == &b'{' {
            inner = idx_end;
            flag = true
        }

        if c == &b'}' && flag {
            flag = false
        }
         
    }

    todo!()
}

fn process_string<'source>(
    marker: &u8,
    idx: usize,
    source: &'source [u8],
    iter: &mut SourceIter<'source>,
    breaker: Option<u8>,
) -> Result<Token<'source>, ParseError> {
    let idx_end = try_consume_until(
        idx,
        iter,
        ParseError::MissingQuotes(Span::new(idx, source.len())),
        |c, _, span| {
            if c == marker {
                ConsumeStatus::Found
            } else {
                match breaker {
                    Some(v) if c == &v => ConsumeStatus::Error(ParseError::MissingQuotes(span)),
                    _ => ConsumeStatus::NotFound,
                }
            }
        },
    )?;

    let chars = &source[idx + 1..idx_end];
    let span = Span::new(idx, idx_end + 1);
    Ok(Token::new(TokenType::String(chars), span))
}

fn process_flag<'source>(
    idx: usize,
    source: &'source [u8],
    iter: &mut SourceIter<'source>,
) -> Token<'source> {
    let size = iter
        .peek()
        .map(|(_, c)| if c == &&b'-' { 2 } else { 1 })
        .unwrap_or_else(|| source.len());

    if size == 2 {
        iter.next();
    }

    let (chars, span) = get_chars(idx, source, iter);
    let chars = &chars[size..];

    Token::new(TokenType::Flag(chars), span)
}

fn process_cellpath<'source>(
    idx: usize,
    source: &'source [u8],
    iter: &mut SourceIter<'source>,
) -> Token<'source> {
    let (chars, span) = get_chars(idx, source, iter);
    let chars = &chars[1..];

    if !chars.contains(&b'.') {
        return Token::new(TokenType::Var(chars), span);
    }

    let mut parts = chars.split(|c| c == &b'.');

    let value = parts
        .next()
        .and_then(|part| if part.is_empty() { None } else { Some(part) });

    let path: Vec<PathType> = parts
        .map(|part| {
            let string_part = String::from_utf8_lossy(part);
            if let Ok(row) = string_part.parse::<usize>() {
                PathType::Row(row)
            } else {
                PathType::Column(part)
            }
        })
        .collect();

    Token::new(TokenType::CellPath { value, path }, span)
}

fn process_item<'source>(
    idx: usize,
    source: &'source [u8],
    iter: &mut SourceIter<'source>,
) -> Token<'source> {
    let (chars, span) = get_chars(idx, source, iter);

    if let Some(token_type) = TokenType::try_keyword(chars) {
        return Token::new(token_type, span);
    }

    let value = String::from_utf8_lossy(chars);

    if let Ok(val) = value.parse::<i64>() {
        return Token::new(TokenType::Int(val), span);
    }

    if let Ok(val) = value.parse::<f64>() {
        return Token::new(TokenType::Float(val), span);
    }

    Token::new(TokenType::Identifier(chars), span)
}

fn process_double_char<'source>(
    default: TokenType<'source>,
    idx: usize,
    iter: &mut SourceIter<'source>,
    options: Vec<(u8, TokenType<'source>)>,
) -> Token<'source> {
    iter.peek()
        .and_then(|(_, next_char)| search_options(next_char, options))
        // Calling next() to move the iterator to the next element
        .and_then(|ttype| iter.next().map(|(idx_next, _)| (idx_next, ttype)))
        .map(|(idx_next, ttype)| Token::new(ttype, Span::new(idx, idx_next + 1)))
        .unwrap_or_else(|| Token::new(default, Span::new(idx, idx + 1)))
}

fn search_options<'source>(
    next_char: &'source u8,
    options: Vec<(u8, TokenType<'source>)>,
) -> Option<TokenType<'source>> {
    for (opt, ttype) in options.into_iter() {
        if next_char == &opt {
            return Some(ttype);
        }
    }

    None
}

#[cfg(test)]
mod test {
    use super::*;
    use std::vec;

    struct Test<'source> {
        source: &'source [u8],
        expected: Vec<Token<'source>>,
        errors: Option<Vec<ParseError>>,
    }

    #[test]
    fn test_chars() {
        let source = b"( ) + [ ] * { };";
        let res = tokenize(source);
        let expected = vec![
            Token::new(TokenType::LeftParen, Span { start: 0, end: 1 }),
            Token::new(TokenType::RightParen, Span { start: 2, end: 3 }),
            Token::new(TokenType::Plus, Span { start: 4, end: 5 }),
            Token::new(TokenType::LeftBracket, Span { start: 6, end: 7 }),
            Token::new(TokenType::RightBracket, Span { start: 8, end: 9 }),
            Token::new(TokenType::Star, Span { start: 10, end: 11 }),
            Token::new(TokenType::LeftBrace, Span { start: 12, end: 13 }),
            Token::new(TokenType::RightBrace, Span { start: 14, end: 15 }),
            Token::new(TokenType::Semicolon, Span { start: 15, end: 16 }),
            Token::new(TokenType::EOF, Span { start: 16, end: 16 }),
        ];

        assert_eq!(res.tokens, expected);
    }

    #[test]
    fn test_pipe() {
        let source = br#"let val = func1
| func2
| func3"#;
        let res = tokenize(source);
        let expected = vec![
            Token::new(TokenType::Let, Span { start: 0, end: 3 }),
            Token::new(TokenType::Identifier(b"val"), Span { start: 4, end: 7 }),
            Token::new(TokenType::Equal, Span { start: 8, end: 9 }),
            Token::new(TokenType::Identifier(b"func1"), Span { start: 10, end: 15 }),
            Token::new(TokenType::Pipe, Span { start: 16, end: 17 }),
            Token::new(TokenType::Identifier(b"func2"), Span { start: 18, end: 23 }),
            Token::new(TokenType::Pipe, Span { start: 24, end: 25 }),
            Token::new(TokenType::Identifier(b"func3"), Span { start: 26, end: 31 }),
            Token::new(TokenType::EOF, Span { start: 31, end: 31 }),
        ];

        assert_eq!(res.tokens, expected);
    }

    #[test]
    fn test_double_chars() {
        let source = br#"= == =~ > >= < <= ! !="#;
        let res = tokenize(source);
        let expected = vec![
            Token::new(TokenType::Equal, Span { start: 0, end: 1 }),
            Token::new(TokenType::EqualEqual, Span { start: 2, end: 4 }),
            Token::new(TokenType::EqualLike, Span { start: 5, end: 7 }),
            Token::new(TokenType::Greater, Span { start: 8, end: 9 }),
            Token::new(TokenType::GreaterEqual, Span { start: 10, end: 12 }),
            Token::new(TokenType::Less, Span { start: 13, end: 14 }),
            Token::new(TokenType::LessEqual, Span { start: 15, end: 17 }),
            Token::new(TokenType::Bang, Span { start: 18, end: 19 }),
            Token::new(TokenType::BangEqual, Span { start: 20, end: 22 }),
            Token::new(TokenType::EOF, Span { start: 22, end: 22 }),
        ];

        assert_eq!(res.tokens, expected);
    }

    #[test]
    fn test_comments() {
        let source = br#"# this is comment one
# this is comment two
#"#;
        let res = tokenize(source);
        let expected = vec![
            Token::new(
                TokenType::Comment(b" this is comment one"),
                Span { start: 0, end: 21 },
            ),
            Token::new(TokenType::EOL, Span { start: 21, end: 22 }),
            Token::new(
                TokenType::Comment(b" this is comment two"),
                Span { start: 22, end: 43 },
            ),
            Token::new(TokenType::EOL, Span { start: 43, end: 44 }),
            Token::new(TokenType::Comment(b""), Span { start: 44, end: 45 }),
            Token::new(TokenType::EOF, Span { start: 45, end: 45 }),
        ];

        assert_eq!(res.tokens, expected);
    }

    #[test]
    fn test_identifier() {
        let tests = vec![
            Test {
                source: b"let val = 15 + 23",
                expected: vec![
                    Token::new(TokenType::Let, Span { start: 0, end: 3 }),
                    Token::new(TokenType::Identifier(b"val"), Span { start: 4, end: 7 }),
                    Token::new(TokenType::Equal, Span { start: 8, end: 9 }),
                    Token::new(TokenType::Int(15), Span { start: 10, end: 12 }),
                    Token::new(TokenType::Plus, Span { start: 13, end: 14 }),
                    Token::new(TokenType::Int(23), Span { start: 15, end: 17 }),
                    Token::new(TokenType::EOF, Span { start: 17, end: 17 }),
                ],
                errors: None,
            },
            Test {
                source: b"alias my_func = if (true) {1} else {2}",
                expected: vec![
                    Token::new(TokenType::Alias, Span { start: 0, end: 5 }),
                    Token::new(
                        TokenType::Identifier(b"my_func"),
                        Span { start: 6, end: 13 },
                    ),
                    Token::new(TokenType::Equal, Span { start: 14, end: 15 }),
                    Token::new(TokenType::If, Span { start: 16, end: 18 }),
                    Token::new(TokenType::LeftParen, Span { start: 19, end: 20 }),
                    Token::new(TokenType::True, Span { start: 20, end: 24 }),
                    Token::new(TokenType::RightParen, Span { start: 24, end: 25 }),
                    Token::new(TokenType::LeftBrace, Span { start: 26, end: 27 }),
                    Token::new(TokenType::Int(1), Span { start: 27, end: 28 }),
                    Token::new(TokenType::RightBrace, Span { start: 28, end: 29 }),
                    Token::new(TokenType::Else, Span { start: 30, end: 34 }),
                    Token::new(TokenType::LeftBrace, Span { start: 35, end: 36 }),
                    Token::new(TokenType::Int(2), Span { start: 36, end: 37 }),
                    Token::new(TokenType::RightBrace, Span { start: 37, end: 38 }),
                    Token::new(TokenType::EOF, Span { start: 38, end: 38 }),
                ],
                errors: None,
            },
        ];

        for t in tests {
            let res = tokenize(t.source);
            assert_eq!(res.tokens, t.expected);
        }
    }

    #[test]
    fn test_cellpath() {
        let tests = vec![
            Test {
                source: b"$var1",
                expected: vec![
                    Token::new(TokenType::Var(b"var1"), Span { start: 0, end: 5 }),
                    Token::new(TokenType::EOF, Span { start: 5, end: 5 }),
                ],
                errors: None,
            },
            Test {
                source: b"$var1.a.1",
                expected: vec![
                    Token::new(
                        TokenType::CellPath {
                            value: Some(b"var1"),
                            path: vec![PathType::Column(b"a"), PathType::Row(1)],
                        },
                        Span { start: 0, end: 9 },
                    ),
                    Token::new(TokenType::EOF, Span { start: 9, end: 9 }),
                ],
                errors: None,
            },
            Test {
                source: b"$.a.1",
                expected: vec![
                    Token::new(
                        TokenType::CellPath {
                            value: None,
                            path: vec![PathType::Column(b"a"), PathType::Row(1)],
                        },
                        Span { start: 0, end: 5 },
                    ),
                    Token::new(TokenType::EOF, Span { start: 5, end: 5 }),
                ],
                errors: None,
            },
        ];

        for t in tests {
            let res = tokenize(t.source);
            assert_eq!(res.tokens, t.expected);
        }
    }

    #[test]
    fn test_flag() {
        let tests = vec![
            Test {
                source: b"2-$a",
                expected: vec![
                    Token::new(TokenType::Int(2), Span { start: 0, end: 1 }),
                    Token::new(TokenType::Minus, Span { start: 1, end: 2 }),
                    Token::new(TokenType::Var(b"a"), Span { start: 2, end: 4 }),
                    Token::new(TokenType::EOF, Span { start: 4, end: 4 }),
                ],
                errors: None,
            },
            Test {
                source: b"func -s --long",
                expected: vec![
                    Token::new(TokenType::Identifier(b"func"), Span { start: 0, end: 4 }),
                    Token::new(TokenType::Flag(b"s"), Span { start: 5, end: 7 }),
                    Token::new(TokenType::Flag(b"long"), Span { start: 8, end: 14 }),
                    Token::new(TokenType::EOF, Span { start: 14, end: 14 }),
                ],
                errors: None,
            },
            Test {
                source: b"-",
                expected: vec![
                    Token::new(TokenType::Minus, Span { start: 0, end: 1 }),
                    Token::new(TokenType::EOF, Span { start: 1, end: 1 }),
                ],
                errors: None,
            },
            Test {
                source: b"--",
                expected: vec![
                    Token::new(TokenType::Flag(b""), Span { start: 0, end: 2 }),
                    Token::new(TokenType::EOF, Span { start: 2, end: 2 }),
                ],
                errors: None,
            },
        ];

        for t in tests {
            let res = tokenize(t.source);
            assert_eq!(res.tokens, t.expected);
        }
    }

    #[test]
    fn test_string() {
        let tests = vec![
            Test {
                source: br#"let str = "this is a string""#,
                expected: vec![
                    Token::new(TokenType::Let, Span { start: 0, end: 3 }),
                    Token::new(TokenType::Identifier(b"str"), Span { start: 4, end: 7 }),
                    Token::new(TokenType::Equal, Span { start: 8, end: 9 }),
                    Token::new(
                        TokenType::String(b"this is a string"),
                        Span { start: 10, end: 28 },
                    ),
                    Token::new(TokenType::EOF, Span { start: 28, end: 28 }),
                ],
                errors: None,
            },
            Test {
                source: br#"let str = "this is a string"#,
                expected: vec![
                    Token::new(TokenType::Let, Span { start: 0, end: 3 }),
                    Token::new(TokenType::Identifier(b"str"), Span { start: 4, end: 7 }),
                    Token::new(TokenType::Equal, Span { start: 8, end: 9 }),
                    Token::new(TokenType::EOF, Span { start: 27, end: 27 }),
                ],
                errors: Some(vec![ParseError::MissingQuotes(Span::new(10, 27))]),
            },
            Test {
                source: br#"let str = "this is a string
let another = "another str"
let error_str = "incomplete str"#,
                expected: vec![
                    Token::new(TokenType::Let, Span { start: 0, end: 3 }),
                    Token::new(TokenType::Identifier(b"str"), Span { start: 4, end: 7 }),
                    Token::new(TokenType::Equal, Span { start: 8, end: 9 }),
                    Token::new(TokenType::Let, Span { start: 28, end: 31 }),
                    Token::new(
                        TokenType::Identifier(b"another"),
                        Span { start: 32, end: 39 },
                    ),
                    Token::new(TokenType::Equal, Span { start: 40, end: 41 }),
                    Token::new(
                        TokenType::String(b"another str"),
                        Span { start: 42, end: 55 },
                    ),
                    Token::new(TokenType::EOL, Span { start: 55, end: 56 }),
                    Token::new(TokenType::Let, Span { start: 56, end: 59 }),
                    Token::new(
                        TokenType::Identifier(b"error_str"),
                        Span { start: 60, end: 69 },
                    ),
                    Token::new(TokenType::Equal, Span { start: 70, end: 71 }),
                    Token::new(TokenType::EOF, Span { start: 87, end: 87 }),
                ],
                errors: Some(vec![
                    ParseError::MissingQuotes(Span::new(10, 27)),
                    ParseError::MissingQuotes(Span::new(72, 87)),
                ]),
            },
            Test {
                source: br#"let str = """#,
                expected: vec![
                    Token::new(TokenType::Let, Span { start: 0, end: 3 }),
                    Token::new(TokenType::Identifier(b"str"), Span { start: 4, end: 7 }),
                    Token::new(TokenType::Equal, Span { start: 8, end: 9 }),
                    Token::new(TokenType::String(b""), Span { start: 10, end: 12 }),
                    Token::new(TokenType::EOF, Span { start: 12, end: 12 }),
                ],
                errors: None,
            },
        ];

        for t in tests {
            let res = tokenize(t.source);
            assert_eq!(res.tokens, t.expected);

            if let Some(errors) = res.errors {
                if let Some(exp_errors) = t.errors {
                    assert_eq!(errors, exp_errors)
                }
            }
        }
    }

    #[test]
    fn test_raw_string() {
        let tests = vec![
            Test {
                source: br#"let str = `raw "string"`"#,
                expected: vec![
                    Token::new(TokenType::Let, Span { start: 0, end: 3 }),
                    Token::new(TokenType::Identifier(b"str"), Span { start: 4, end: 7 }),
                    Token::new(TokenType::Equal, Span { start: 8, end: 9 }),
                    Token::new(
                        TokenType::String(br#"raw "string""#),
                        Span { start: 10, end: 24 },
                    ),
                    Token::new(TokenType::EOF, Span { start: 24, end: 24 }),
                ],
                errors: None,
            },
            Test {
                source: br#"let str = `raw "string"`
let another = `another str`"#,
                expected: vec![
                    Token::new(TokenType::Let, Span { start: 0, end: 3 }),
                    Token::new(TokenType::Identifier(b"str"), Span { start: 4, end: 7 }),
                    Token::new(TokenType::Equal, Span { start: 8, end: 9 }),
                    Token::new(
                        TokenType::String(br#"raw "string""#),
                        Span { start: 10, end: 24 },
                    ),
                    Token::new(TokenType::EOL, Span { start: 24, end: 25 }),
                    Token::new(TokenType::Let, Span { start: 25, end: 28 }),
                    Token::new(
                        TokenType::Identifier(b"another"),
                        Span { start: 29, end: 36 },
                    ),
                    Token::new(TokenType::Equal, Span { start: 37, end: 38 }),
                    Token::new(
                        TokenType::String(b"another str"),
                        Span { start: 39, end: 52 },
                    ),
                    Token::new(TokenType::EOF, Span { start: 52, end: 52 }),
                ],
                errors: None,
            },
            Test {
                ////////////000000000011111111112222222
                ////////////012345678901234567890123456
                source: br#"let str = `raw "string""#,
                expected: vec![
                    Token::new(TokenType::Let, Span { start: 0, end: 3 }),
                    Token::new(TokenType::Identifier(b"str"), Span { start: 4, end: 7 }),
                    Token::new(TokenType::Equal, Span { start: 8, end: 9 }),
                    Token::new(TokenType::EOF, Span { start: 23, end: 23 }),
                ],
                errors: Some(vec![ParseError::MissingQuotes(Span::new(10, 23))]),
            },
        ];

        for t in tests {
            let res = tokenize(t.source);
            assert_eq!(res.tokens, t.expected);

            if let Some(errors) = res.errors {
                if let Some(exp_errors) = t.errors {
                    assert_eq!(errors, exp_errors)
                }
            }
        }
    }
}
