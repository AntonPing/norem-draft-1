use crate::position::{Position, Span, Spanned};
use std::fmt;
use std::str::Chars;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenKind {
    /// "("
    LParen,
    /// ")"
    RParen,
    /// "["
    LBracket,
    /// "]"
    RBracket,
    /// "{"
    LBrace,
    /// "}"
    RBrace,
    /// ":"
    Colon,
    /// ";"
    Semi,
    /// ","
    Comma,
    /// "."
    Dot,
    /// "|"
    Bar,
    /// "="
    Equal,
    /// "->"
    Arrow,
    /// "=>"
    EArrow,
    /// "fun"
    Fun,
    /// "let"
    Let,
    /// "begin"
    Begin,
    /// "in"
    In,
    /// "end"
    End,
    /// "case"
    Case,
    /// "of"
    Of,
    /// "data"
    Data,
    /// "type"
    Type,
    /// "if"
    If,
    /// "then"
    Then,
    /// "else"
    Else,
    /// literal value `Int`
    LitInt,
    /// literal value `Real`
    LitReal,
    /// literal value `Bool`
    LitBool,
    /// literal value `Char`
    LitChar,
    /// literal type `Int`
    TyInt,
    /// literal type `Real`
    TyReal,
    /// literal type `Bool`
    TyBool,
    /// literal type `Char`
    TyChar,
    /// literal type or value `Unit`
    Unit,
    /// builtin primitives
    Builtin,
    /// identifier(lowercase)
    LowerIdent,
    /// identifier(uppercase)
    UpperIdent,
    /// wildcard "_", could be something like "_foo"
    Wild,
    /// user-defined operator
    Oper,
    /// lexer failed, skip till next whitespace
    FailedToken,
    // failed block comments
    FailedBlockComment,

    // following token kinds should be use only inside lexer
    // they are not intended to be exposed to parser

    // line comments
    LineComment,
    // block comments
    BlockComment,
    // end of file
    EndOfFile,
}

impl TokenKind {
    pub fn is_bad_token(&self) -> bool {
        match self {
            TokenKind::FailedToken
            | TokenKind::FailedBlockComment
            | TokenKind::LineComment
            | TokenKind::BlockComment
            | TokenKind::EndOfFile => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> fmt::Result {
        write!(f, "<{:?}> {:?}", self.kind, self.span)
    }
}

impl Spanned for Token {
    fn span(&self) -> &Span {
        &self.span
    }
    fn span_mut(&mut self) -> &mut Span {
        &mut self.span
    }
}

pub fn as_keyword(str: &str) -> Option<TokenKind> {
    let tok = match str {
        "fun" => TokenKind::Fun,
        "let" => TokenKind::Let,
        "begin" => TokenKind::Begin,
        "in" => TokenKind::In,
        "end" => TokenKind::End,
        "case" => TokenKind::Case,
        "of" => TokenKind::Of,
        "if" => TokenKind::If,
        "then" => TokenKind::Then,
        "else" => TokenKind::Else,
        "data" => TokenKind::Data,
        "type" => TokenKind::Type,
        "true" => TokenKind::LitBool,
        "false" => TokenKind::LitBool,
        "Int" => TokenKind::TyInt,
        "Real" => TokenKind::TyReal,
        "Bool" => TokenKind::TyBool,
        "Char" => TokenKind::TyChar,
        _ => {
            return None;
        }
    };
    Some(tok)
}

pub fn is_ident_first(ch: char) -> bool {
    ch.is_ascii_alphabetic()
}

pub fn is_ident_body(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '-' || ch == '_'
}

pub fn is_opr_char(ch: char) -> bool {
    match ch {
        ':' | '!' | '#' | '$' | '%' | '&' | '*' | '+' | '.' | '/' | '<' | '=' | '>' | '?' | '@'
        | '\\' | '^' | '|' | '-' | '~' => true,
        _ => false,
    }
}

pub struct Lexer<'src> {
    source: &'src str,
    chars: Chars<'src>,
    row: usize,
    col: usize,
    abs: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(s: &'src str) -> Self {
        Lexer {
            source: s,
            chars: s.chars(),
            row: 0,
            col: 0,
            abs: 0,
        }
    }

    fn peek_first(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn peek_second(&self) -> Option<char> {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next()
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.chars.next()?;
        self.abs += ch.len_utf8();
        if ch == '\n' {
            self.row += 1;
            self.col = 0;
        } else {
            self.col += ch.len_utf8();
        }
        Some(ch)
    }

    fn get_pos(&self) -> Position {
        Position::new(self.row, self.col, self.abs)
    }

    fn get_abs(&self) -> usize {
        self.abs
    }

    fn get_slice(&self, start: usize, end: usize) -> &'src str {
        &self.source[start..end]
    }

    fn skip_while(&mut self, f: fn(char) -> bool) -> usize {
        let mut n = 0;
        while let Some(ch) = self.peek_first() {
            if f(ch) {
                self.next_char();
                n += 1;
            } else {
                break;
            }
        }
        n
    }

    pub fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek_first() {
            if ch.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        loop {
            self.skip_whitespace();
            let start = self.get_pos();
            let kind = self.next_token_kind();
            match kind {
                TokenKind::LineComment | TokenKind::BlockComment => {
                    // ignore comments, they will never be exposed to parser
                    continue;
                }
                TokenKind::EndOfFile => {
                    return None;
                }
                _ => {}
            }
            let end = self.get_pos();
            let span = Span::new(start, end);
            return Some(Token { kind, span });
        }
    }

    pub fn next_token_kind(&mut self) -> TokenKind {
        match self.peek_first() {
            Some('(') => match self.peek_second() {
                Some(')') => {
                    self.next_char();
                    self.next_char();
                    TokenKind::Unit
                }
                _ => {
                    self.next_char();
                    TokenKind::LParen
                }
            },
            Some(')') => {
                self.next_char();
                TokenKind::RParen
            }
            Some('[') => {
                self.next_char();
                TokenKind::LBracket
            }
            Some(']') => {
                self.next_char();
                TokenKind::RBracket
            }
            Some('{') => {
                self.next_char();
                TokenKind::LBrace
            }
            Some('}') => {
                self.next_char();
                TokenKind::RBrace
            }
            Some(':') => {
                self.next_char();
                TokenKind::Colon
            }
            Some(';') => {
                self.next_char();
                TokenKind::Semi
            }
            Some(',') => {
                self.next_char();
                TokenKind::Comma
            }
            Some('.') => {
                self.next_char();
                TokenKind::Dot
            }
            Some('|') => {
                self.next_char();
                TokenKind::Bar
            }
            Some('=') => match self.peek_second() {
                Some('>') => {
                    self.next_char();
                    self.next_char();
                    TokenKind::EArrow
                }
                _ => {
                    self.next_char();
                    TokenKind::Equal
                }
            },
            Some('-') => match self.peek_second() {
                Some('>') => {
                    self.next_char();
                    self.next_char();
                    TokenKind::Arrow
                }
                _ => self.operator(),
            },
            Some('/') => match self.peek_second() {
                Some('/') => {
                    self.line_comment();
                    TokenKind::LineComment
                }
                Some('*') => {
                    if self.block_comment() {
                        TokenKind::BlockComment
                    } else {
                        TokenKind::FailedBlockComment
                    }
                }
                _ => self.operator(),
            },
            Some('@') => self.builtin(),
            Some('_') => self.wildcard(),
            Some(ch) if is_opr_char(ch) => self.operator(),
            Some(ch) if is_ident_first(ch) => self.ident_or_keyword(),
            Some(ch) if ch.is_ascii_digit() => self.int_or_real(),
            Some(_) => self.failed_token(),
            None => TokenKind::EndOfFile,
        }
    }

    fn block_comment(&mut self) -> bool {
        let ch1 = self.next_char().unwrap();
        assert_eq!(ch1, '/');
        let ch2 = self.next_char().unwrap();
        assert_eq!(ch2, '*');
        loop {
            match self.peek_first() {
                Some('*') if self.peek_second() == Some('/') => {
                    self.next_char();
                    self.next_char();
                    return true;
                }
                Some('/') if self.peek_second() == Some('*') => {
                    if !self.block_comment() {
                        return false;
                    }
                }
                Some(_) => {
                    self.next_char();
                }
                None => {
                    return false;
                }
            }
        }
    }

    fn line_comment(&mut self) {
        let ch1 = self.next_char().unwrap();
        assert_eq!(ch1, '/');
        let ch2 = self.next_char().unwrap();
        assert_eq!(ch2, '/');
        loop {
            match self.next_char() {
                Some('\n') | None => {
                    return;
                }
                Some(_) => {}
            }
        }
    }

    fn ident_or_keyword(&mut self) -> TokenKind {
        let start = self.get_abs();
        let ch1 = self.next_char().unwrap();
        assert!(is_ident_first(ch1));
        self.skip_while(|ch| is_ident_body(ch));
        let end = self.get_abs();
        let slice = self.get_slice(start, end);
        if let Some(kwd) = as_keyword(slice) {
            kwd
        } else {
            if ch1.is_ascii_lowercase() {
                TokenKind::LowerIdent
            } else {
                TokenKind::UpperIdent
            }
        }
    }

    fn wildcard(&mut self) -> TokenKind {
        let ch1 = self.next_char();
        assert_eq!(ch1, Some('_'));
        self.skip_while(|ch| ch.is_alphanumeric());
        TokenKind::Wild
    }

    fn builtin(&mut self) -> TokenKind {
        let ch1 = self.next_char();
        assert_eq!(ch1, Some('@'));
        self.skip_while(|ch| ch.is_alphanumeric());
        TokenKind::Builtin
    }

    fn operator(&mut self) -> TokenKind {
        let len = self.skip_while(is_opr_char);
        assert_ne!(len, 0);
        TokenKind::Oper
    }

    fn int_or_real(&mut self) -> TokenKind {
        // note: we don't accept forms like "3." or ".14"
        let len = self.skip_while(|ch| ch.is_ascii_digit());
        assert_ne!(len, 0);
        if self.peek_first() != Some('.') {
            TokenKind::LitInt
        } else {
            self.next_char();
            let len = self.skip_while(|ch| ch.is_ascii_digit());
            if len == 0 {
                TokenKind::FailedToken
            } else {
                TokenKind::LitReal
            }
        }
    }

    fn failed_token(&mut self) -> TokenKind {
        // ignore all char until a whitespace
        self.skip_while(|ch| !ch.is_whitespace());
        TokenKind::FailedToken
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[test]
fn lexer_test() {
    let string = r#"
begin
    type My-Int = Int;
    type Option-Int = Option[Int];
    data Option[T] =
    | Some(T)
    | None
    end
    fun add1(x) => @iadd(x, 1)
    fun add2(x) =>
        let y = @iadd(x,1);
        @iadd(y,1)
    fun const-3(x) => {
        let y = @iadd(x,1);
        @iadd(y,1)
    }
    fun option-add1(x) =>
        case x of
        | Some(y) => { Some(@iadd(x,1)) }
        | None => { None }
        end
in
    @isub(addone(42), 1)
end
"#;
    let lex = Lexer::new(string);
    lex.into_iter().for_each(|tok| {
        assert!(!tok.kind.is_bad_token());
    });
}
