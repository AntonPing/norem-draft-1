use crate::ast::*;
use crate::position::{Span, Position, Spanned};
use crate::intern::{InternStr, intern};
use crate::lexer::{Token, Lexer, TokenKind};

pub struct Parser<'src> {
    source: &'src str,
    tokens: Vec<Token>,
    cursor: usize,
    // for error message
    error: Vec<ParseError>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseError {
    LexerError(Span, &'static str),
    Unexpected(Span, TokenKind, TokenKind),
    UnexpectedMany(Span, TokenKind, &'static [TokenKind]),
    UnknownBuiltin(Span, InternStr),
}

type ParseFunc<T> = fn(&mut Parser) -> Option<T>;

impl<'src> Parser<'src> {
    pub fn new(input: &'src str) -> Parser<'src> {
        let lex = Lexer::new(input);
        let tokens: Vec<Token> = lex.into_iter().collect();
        Parser {
            source: input,
            tokens: tokens,
            cursor: 0,
            error: Vec::new(),
        }
    }

    fn peek_token(&self) -> &Token {
        &self.tokens[self.cursor]
    }

    fn peek_kind(&self) -> TokenKind {
        self.tokens[self.cursor].kind
    }

    fn peek_span(&self) -> &Span {
        &self.tokens[self.cursor].span
    }

    fn start_pos(&self) -> Position {
        self.tokens[self.cursor].span.start
    }

    fn end_pos(&self) -> Position {
        self.tokens[self.cursor - 1].span.end
    }

    fn peek_slice(&self) -> &'src str {
        let span = &self.tokens[self.cursor].span;
        let start = span.start.abs;
        let end = span.end.abs;
        &self.source[start..end]
    }

    fn next_token(&mut self) -> &Token {
        let tok = &self.tokens[self.cursor];
        if self.cursor < self.tokens.len() {
            self.cursor += 1;
        }
        tok
    }

    fn emit_unexpected(&mut self, token: TokenKind) {
        let Token { kind, span } = self.peek_token();
        let err = ParseError::Unexpected(*span, *kind, token);
        self.error.push(err);
    }

    fn emit_unexpected_many(&mut self, vec: &'static [TokenKind]) {
        let Token { kind, span } = self.peek_token();
        let err = ParseError::UnexpectedMany(*span, *kind, vec);
        self.error.push(err);
    }

    fn match_token(&mut self, token: TokenKind) -> Option<()> {
        if self.peek_kind() == token {
            self.next_token();
            Some(())
        } else {
            self.emit_unexpected(token);
            None
        }
    }

    fn match_int(&mut self) -> Option<i64> {
        if self.peek_kind() == TokenKind::LitInt {
            let slice = self.peek_slice();
            self.next_token();
            Some(slice.parse().unwrap())
        } else {
            self.emit_unexpected(TokenKind::LitInt);
            None
        }
    }

    fn match_literal(&mut self) -> Option<LitVal> {
        match self.peek_kind() {
            TokenKind::LitInt => {
                let slice = self.peek_slice();
                self.next_token();
                Some(LitVal::Int(slice.parse().unwrap()))
            }
            TokenKind::LitReal => {
                let slice = self.peek_slice();
                self.next_token();
                Some(LitVal::Real(slice.parse().unwrap()))
            }
            TokenKind::LitBool => {
                let slice = self.peek_slice();
                self.next_token();
                Some(LitVal::Bool(slice.parse().unwrap()))
            }
            TokenKind::LitChar => {
                let slice = self.peek_slice();
                self.next_token();
                Some(LitVal::Bool(slice.parse().unwrap()))
            }
            _ => {
                static VEC: [TokenKind; 4] = [
                    TokenKind::LitInt, TokenKind::LitReal, TokenKind::LitBool, TokenKind::LitChar
                ];
                self.emit_unexpected_many(&VEC);
                None
            }
        }
    }

    fn match_ident(&mut self) -> Option<InternStr> {
        if self.peek_kind() == TokenKind::Ident {
            let slice = self.peek_slice();
            self.next_token();
            Some(intern(slice))
        } else {
            self.emit_unexpected(TokenKind::Ident);
            None
        }
    }

    fn match_builtin(&mut self) -> Option<Builtin> {
        if self.peek_kind() == TokenKind::Builtin {
            let slice = self.peek_slice();
            self.next_token();
            let res = match slice {
                "@iadd" => Builtin::IAdd,
                "@isub" => Builtin::ISub,
                "@imul" => Builtin::IMul,
                "@idiv" => Builtin::IDiv,
                "@irem" => Builtin::IRem,
                "@ineg" => Builtin::INeg,
                "@radd" => Builtin::RAdd,
                "@rsub" => Builtin::RSub,
                "@rmul" => Builtin::RMul,
                "@rdiv" => Builtin::RDiv,
                "@band" => Builtin::BAnd,
                "@bor" => Builtin::BOr,
                "@bnot" => Builtin::BNot,
                _ => {
                    let span = *self.peek_span();
                    self.error.push(ParseError::UnknownBuiltin(span, intern(slice)));
                    return None;
                }
            };
            Some(res)
        } else {
            self.emit_unexpected(TokenKind::Builtin);
            None
        }
    }

    fn many<T>(&mut self, func: ParseFunc<T>) -> Option<Vec<T>> {      
        let mut vec = Vec::new();
        let mut last = self.cursor;
        loop {
            match func(self) {
                Some(res) => {
                    vec.push(res);
                    last = self.cursor;
                }
                None => {
                    // if it failed without consuming any token
                    if self.cursor == last {
                        // return the result
                        return Some(vec);
                    } else {
                        // rethrow the error
                        return None;
                    }
                }
            }
        }
    }

    fn many1<T>(&mut self, func: ParseFunc<T>) -> Option<Vec<T>> {
        let first = func(self)?;
        let mut vec = self.many(func)?;
        vec.insert(0, first);
        Some(vec)
    }

    fn sepby<T>(&mut self, delim: TokenKind, func: ParseFunc<T>) -> Option<Vec<T>> {
        let last = self.cursor;
        match self.sepby1(delim, func) {
            Some(res) => Some(res),
            None => {
                if self.cursor == last {
                    Some(Vec::new())
                } else {
                    None
                }
            }
        }
    }

    fn sepby1<T>(&mut self, delim: TokenKind, func: ParseFunc<T>) -> Option<Vec<T>> {
        let mut vec = Vec::new();
        let first = func(self)?;
        vec.push(first);
        while self.peek_kind() == delim {
            self.next_token();
            let res = func(self)?;
            vec.push(res);
        }
        Some(vec)
    }
}

pub fn parse_expr(p: &mut Parser) -> Option<Expr> {
    let start = p.start_pos();
    let expr = parse_expr_no_app(p)?;
    if p.peek_kind() != TokenKind::LParen {
        return Some(expr);
    }

    p.match_token(TokenKind::LParen)?;
    let args = p.sepby(TokenKind::Comma, parse_expr)?;
    p.match_token(TokenKind::RParen)?;
    
    let end = p.end_pos();
    let span = Span::new(start, end);

    Some(Expr::App {
        func: Box::new(expr),
        args,
        span,
    })
}

fn parse_expr_no_app(p: &mut Parser) -> Option<Expr> {
    let start = p.start_pos();
    match p.peek_kind() {
        TokenKind::Ident => {
            let span = *p.peek_span();
            let var = p.match_ident().unwrap();
            Some(Expr::Var { var, span })
        }
        TokenKind::LitInt | TokenKind::LitReal | TokenKind::LitBool | TokenKind::LitChar => {
            let span = *p.peek_span();
            let lit = p.match_literal().unwrap();
            Some(Expr::Lit { lit, span })
        }
        TokenKind::LParen => {
            p.match_token(TokenKind::LParen).unwrap();
            let mut expr = parse_expr(p)?;
            p.match_token(TokenKind::RParen)?;
            *expr.span_mut() = Span::new(start,p.end_pos());
            Some(expr)
        }
        TokenKind::Fn => {
            p.match_token(TokenKind::Fn).unwrap();
            p.match_token(TokenKind::LParen)?;
            let pars = p.sepby(TokenKind::Comma, |p| p.match_ident())?;
            p.match_token(TokenKind::RParen)?;
            p.match_token(TokenKind::LBrace)?;
            let body = Box::new(parse_expr(p)?);
            p.match_token(TokenKind::RBrace)?;
            let span = Span::new(start,p.end_pos());
            Some(Expr::Fun { pars, body, span })
        }
        TokenKind::Let => {
            p.match_token(TokenKind::Let).unwrap();
            let bind = p.match_ident()?;
            p.match_token(TokenKind::Equal)?;
            let expr = Box::new(parse_expr(p)?);
            p.match_token(TokenKind::Semi)?;
            let cont = Box::new(parse_expr(p)?);
            let span = Span::new(start,p.end_pos());
            Some(Expr::Let { bind, expr, cont, span })
        }
        TokenKind::Builtin => {
            let op = p.match_builtin().unwrap();
            p.match_token(TokenKind::LParen)?;
            let args = p.sepby(TokenKind::Comma, parse_expr)?;
            p.match_token(TokenKind::RParen)?;
            let span = Span::new(start,p.end_pos());
            Some(Expr::Opr { op, args, span })
        }
        _ => {
            static VEC: &[TokenKind] = &[
                TokenKind::Ident, TokenKind::LitInt, TokenKind::LitReal, TokenKind::LitBool, TokenKind::LitChar,
                TokenKind::LParen, TokenKind::Fn, TokenKind::Let, TokenKind::Builtin,
            ];
            p.emit_unexpected_many(VEC);
            None
        }
    }
}

pub fn parse_decl(p: &mut Parser) -> Option<Decl> {
    match p.peek_kind() {
        _ => {
            todo!()
        }
    }
}

#[test]
fn parser_test() {
    let string = "
        fn (x,y) { @iadd(x,y) }(1,2)
    ";

    let mut par = Parser::new(string);
    let res = parse_expr(&mut par);
    match res {
        Some(res) => {
            println!("{}", res);
        }
        None => {
            println!("{:#?}", par.error);
        }
    }
}