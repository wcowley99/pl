use std::{
    collections::{HashMap, VecDeque},
    iter::Peekable,
    str::Chars,
};

#[derive(Debug, Clone, PartialEq, PartialOrd, Ord, Eq)]
pub enum Lexeme {
    // Single-character tokens
    LParen,
    RParen,
    Comma,
    Dot,
    Eq,
    Plus,
    Minus,
    Star,
    Slash,

    // Literals
    Id(String),
    Str(String),
    Num(i64),

    // Keywords
    Inc,
    Let,
    In,
    If,
    Then,
    Else,
}

impl Lexeme {
    fn id<S: Into<String>>(name: S) -> Lexeme {
        Lexeme::Id(name.into())
    }
}

pub struct Lex {
    lexemes: VecDeque<Lexeme>,
}

impl Lex {
    pub fn new<S: Into<String>>(input: S) -> Self {
        let mut tokens = vec![];

        let source: String = input.into();
        let mut chars = source.chars().peekable();

        let single_chars = HashMap::from([
            ('(', Lexeme::LParen),
            (')', Lexeme::RParen),
            (',', Lexeme::Comma),
            ('.', Lexeme::Dot),
            ('=', Lexeme::Eq),
            ('+', Lexeme::Plus),
            ('*', Lexeme::Star),
        ]);

        while let Some(c) = chars.peek() {
            match c {
                '(' | ')' | ',' | '.' | '=' | '+' | '*' => {
                    tokens.push(single_chars.get(c).unwrap().clone());
                    _ = chars.next();
                }
                '"' => tokens.push(next_string(&mut chars)),
                ' ' => _ = chars.next(),
                c => {
                    if c.is_numeric() {
                        tokens.push(next_number(&mut chars));
                    } else if c.is_alphabetic() || *c == '_' {
                        tokens.push(next_identifier(&mut chars));
                    } else {
                        _ = chars.next()
                    }
                }
            }
        }

        Self {
            lexemes: VecDeque::from(tokens),
        }
    }

    pub fn peek(&self) -> Option<&Lexeme> {
        self.lexemes.get(0)
    }

    pub fn next(&mut self) -> Option<Lexeme> {
        self.lexemes.pop_front()
    }

    pub fn consume(&mut self) {
        self.lexemes.pop_front();
    }

    pub fn require_next(&mut self) -> Result<Lexeme, String> {
        self.next().ok_or("Expected Some token, found None".into())
    }
}

fn next_number(chars: &mut Peekable<Chars>) -> Lexeme {
    let mut builder = String::new();
    while let Some(c) = chars.peek() {
        if c.is_numeric() {
            builder.push(chars.next().expect("Failed with non-empty iterator"));
        } else {
            break;
        }
    }

    Lexeme::Num(builder.parse().expect("Failed to parse String as i64"))
}

fn next_string(chars: &mut Peekable<Chars>) -> Lexeme {
    let mut builder = String::new();
    _ = chars.next(); // discard opening quotation
    while let Some(c) = chars.peek() {
        if *c == '"' {
            break;
        } else {
            builder.push(chars.next().expect("Failed with non-empty iterator"));
        }
    }

    Lexeme::Str(builder)
}

fn next_identifier(chars: &mut Peekable<Chars>) -> Lexeme {
    let mut builder = String::new();
    while let Some(c) = chars.peek() {
        if c.is_alphanumeric() || *c == '_' {
            builder.push(chars.next().expect("Failed with non-empty iterator"));
        } else {
            break;
        }
    }

    match builder.as_str() {
        "inc" => Lexeme::Inc,
        "let" => Lexeme::Let,
        "in" => Lexeme::In,
        "if" => Lexeme::If,
        "then" => Lexeme::Then,
        "else" => Lexeme::Else,
        _ => Lexeme::Id(builder),
    }
}

#[cfg(test)]
mod test {
    use std::collections::VecDeque;

    use crate::lex::{Lex, Lexeme};

    #[test]
    fn lex_numbers() {
        let lex = Lex::new("5");
        assert_eq!(lex.lexemes, VecDeque::from(vec![Lexeme::Num(5)]));

        let lex = Lex::new("24");
        assert_eq!(lex.lexemes, VecDeque::from(vec![Lexeme::Num(24)]));

        let lex = Lex::new("123456789");
        assert_eq!(lex.lexemes, VecDeque::from(vec![Lexeme::Num(123456789)]));
    }

    #[test]
    fn lexer_identifiers() {
        let lex = Lex::new("x");
        assert_eq!(lex.lexemes, VecDeque::from(vec![Lexeme::id("x")]));

        let lex = Lex::new("_validId123");
        assert_eq!(lex.lexemes, VecDeque::from(vec![Lexeme::id("_validId123")]));

        let lex = Lex::new("_super_1ong_val1d_1d_123");
        assert_eq!(
            lex.lexemes,
            VecDeque::from(vec![Lexeme::id("_super_1ong_val1d_1d_123")])
        );
    }

    #[test]
    fn lex_basic() {
        let lex = Lex::new("1 + 2 + 34");

        assert_eq!(
            lex.lexemes,
            VecDeque::from(vec![
                Lexeme::Num(1),
                Lexeme::Plus,
                Lexeme::Num(2),
                Lexeme::Plus,
                Lexeme::Num(34)
            ])
        );
    }

    #[test]
    fn lex_basic_no_whitespace() {
        let lex = Lex::new("1+2+34");

        assert_eq!(
            lex.lexemes,
            VecDeque::from(vec![
                Lexeme::Num(1),
                Lexeme::Plus,
                Lexeme::Num(2),
                Lexeme::Plus,
                Lexeme::Num(34)
            ])
        );
    }

    #[test]
    fn lex_if_keywords() {
        let lex = Lex::new("(if if 5 then 1 else 0 else 6)");

        assert_eq!(
            lex.lexemes,
            VecDeque::from(vec![
                Lexeme::LParen,
                Lexeme::If,
                Lexeme::If,
                Lexeme::Num(5),
                Lexeme::Then,
                Lexeme::Num(1),
                Lexeme::Else,
                Lexeme::Num(0),
                Lexeme::Else,
                Lexeme::Num(6),
                Lexeme::RParen,
            ])
        );
    }

    #[test]
    fn lex_let_keywords() {
        let lex = Lex::new("let x = let y = 1 in y * y in (x + x)");

        assert_eq!(
            lex.lexemes,
            VecDeque::from(vec![
                Lexeme::Let,
                Lexeme::id("x"),
                Lexeme::Eq,
                Lexeme::Let,
                Lexeme::id("y"),
                Lexeme::Eq,
                Lexeme::Num(1),
                Lexeme::In,
                Lexeme::id("y"),
                Lexeme::Star,
                Lexeme::id("y"),
                Lexeme::In,
                Lexeme::LParen,
                Lexeme::id("x"),
                Lexeme::Plus,
                Lexeme::id("x"),
                Lexeme::RParen,
            ])
        );
    }
}
