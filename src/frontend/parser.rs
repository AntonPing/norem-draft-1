use super::lexer::{Lexer, Token, TokenKind};
use super::*;

pub struct Parser<'src> {
    source: &'src str,
    tokens: Vec<Token>,
    cursor: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ParseError {
    LexerError(Span, &'static str),
    Unexpected(Span, TokenKind, TokenKind),
    UnexpectedMany(Span, TokenKind, &'static [TokenKind]),
    UnknownBuiltin(Span, InternStr),
}

type ParseResult<T> = Result<T, ParseError>;
type ParseFunc<T> = fn(&mut Parser) -> ParseResult<T>;

impl<'src> Parser<'src> {
    pub fn new(input: &'src str) -> Parser<'src> {
        let lex = Lexer::new(input);
        let tokens: Vec<Token> = lex.into_iter().collect();
        Parser {
            source: input,
            tokens,
            cursor: 0,
        }
    }

    fn peek_token(&self) -> &Token {
        &self.tokens[self.cursor]
    }

    fn peek_first(&self) -> TokenKind {
        self.tokens[self.cursor].kind
    }

    fn peek_second(&self) -> TokenKind {
        if self.cursor < self.tokens.len() - 1 {
            self.tokens[self.cursor + 1].kind
        } else {
            TokenKind::EndOfFile
        }
    }

    fn peek_span(&self) -> &Span {
        &self.tokens[self.cursor].span
    }

    fn peek_slice(&self) -> &'src str {
        let span = &self.tokens[self.cursor].span;
        &self.source[span.start.abs..span.end.abs]
    }

    fn start_pos(&self) -> Position {
        self.tokens[self.cursor].span.start
    }

    fn end_pos(&self) -> Position {
        self.tokens[self.cursor - 1].span.end
    }

    fn next_token(&mut self) -> &Token {
        let tok = &self.tokens[self.cursor];
        if self.cursor < self.tokens.len() - 1 {
            self.cursor += 1;
        }
        tok
    }

    fn backup(&self) -> usize {
        self.cursor
    }

    fn recover(&mut self, rec: usize) {
        self.cursor = rec;
    }

    fn err_unexpected(&mut self, token: TokenKind) -> ParseError {
        let Token { kind, span } = self.peek_token();
        ParseError::Unexpected(*span, *kind, token)
    }

    fn err_unexpected_many(&mut self, vec: &'static [TokenKind]) -> ParseError {
        let Token { kind, span } = self.peek_token();
        ParseError::UnexpectedMany(*span, *kind, vec)
    }

    fn match_token(&mut self, token: TokenKind) -> ParseResult<()> {
        if self.peek_first() == token {
            self.next_token();
            Ok(())
        } else {
            Err(self.err_unexpected(token))
        }
    }

    /*
    fn match_int(&mut self) -> ParseResult<i64> {
        if self.peek_kind() == TokenKind::LitInt {
            let slice = self.peek_slice();
            self.next_token();
            Ok(slice.parse().unwrap())
        } else {
            Err(self.err_unexpected(TokenKind::LitInt))
        }
    }
    */

    fn match_lit_val(&mut self) -> ParseResult<LitVal> {
        match self.peek_first() {
            TokenKind::LitInt => {
                let slice = self.peek_slice();
                self.next_token();
                Ok(LitVal::Int(slice.parse().unwrap()))
            }
            TokenKind::LitReal => {
                let slice = self.peek_slice();
                self.next_token();
                Ok(LitVal::Real(slice.parse().unwrap()))
            }
            TokenKind::LitBool => {
                let slice = self.peek_slice();
                self.next_token();
                Ok(LitVal::Bool(slice.parse().unwrap()))
            }
            TokenKind::LitChar => {
                let slice = self.peek_slice();
                self.next_token();
                Ok(LitVal::Bool(slice.parse().unwrap()))
            }
            TokenKind::LParen if self.peek_second() == TokenKind::RParen => {
                self.next_token();
                self.next_token();
                Ok(LitVal::Unit)
            }
            _ => {
                static VEC: &[TokenKind] = &[
                    TokenKind::LitInt,
                    TokenKind::LitReal,
                    TokenKind::LitBool,
                    TokenKind::LitChar,
                    TokenKind::LParen,
                ];
                Err(self.err_unexpected_many(VEC))
            }
        }
    }

    fn match_lit_type(&mut self) -> ParseResult<LitType> {
        match self.peek_first() {
            TokenKind::TyInt => {
                self.next_token();
                Ok(LitType::Int)
            }
            TokenKind::TyReal => {
                self.next_token();
                Ok(LitType::Real)
            }
            TokenKind::TyBool => {
                self.next_token();
                Ok(LitType::Bool)
            }
            TokenKind::TyChar => {
                self.next_token();
                Ok(LitType::Char)
            }
            TokenKind::LParen if self.peek_second() == TokenKind::RParen => {
                self.next_token();
                self.next_token();
                Ok(LitType::Unit)
            }
            _ => {
                static VEC: &[TokenKind] = &[
                    TokenKind::TyInt,
                    TokenKind::TyReal,
                    TokenKind::TyBool,
                    TokenKind::TyChar,
                    TokenKind::LParen,
                ];
                Err(self.err_unexpected_many(VEC))
            }
        }
    }

    fn match_lower_ident(&mut self) -> ParseResult<Ident> {
        if self.peek_first() == TokenKind::LowerIdent {
            let slice = self.peek_slice();
            self.next_token();
            Ok(Ident::from(InternStr::new(slice)))
        } else {
            Err(self.err_unexpected(TokenKind::LowerIdent))
        }
    }

    fn match_upper_ident(&mut self) -> ParseResult<Ident> {
        if self.peek_first() == TokenKind::UpperIdent {
            let slice = self.peek_slice();
            self.next_token();
            Ok(Ident::from(InternStr::new(slice)))
        } else {
            Err(self.err_unexpected(TokenKind::UpperIdent))
        }
    }

    fn match_builtin(&mut self) -> ParseResult<Builtin> {
        if self.peek_first() == TokenKind::Builtin {
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
                "@icmpeq" => Builtin::ICmpEq,
                "@icmpne" => Builtin::ICmpNe,
                "@icmpgr" => Builtin::ICmpGr,
                "@icmpge" => Builtin::ICmpGe,
                "@icmpls" => Builtin::ICmpLs,
                "@icmple" => Builtin::ICmpLe,
                "@rcmpeq" => Builtin::RCmpEq,
                "@rcmpne" => Builtin::RCmpNe,
                "@rcmpgr" => Builtin::RCmpGr,
                "@rcmpge" => Builtin::RCmpGe,
                "@rcmpls" => Builtin::RCmpLs,
                "@rcmple" => Builtin::RCmpLe,
                _ => {
                    let span = *self.peek_span();
                    let err = ParseError::UnknownBuiltin(span, InternStr::new(slice));
                    return Err(err);
                }
            };
            Ok(res)
        } else {
            Err(self.err_unexpected(TokenKind::Builtin))
        }
    }

    fn option<T>(&mut self, func: ParseFunc<T>) -> ParseResult<Option<T>> {
        let last = self.cursor;
        match func(self) {
            Ok(res) => Ok(Some(res)),
            Err(err) => {
                // if it failed without consuming any token
                if self.cursor == last {
                    Ok(None) // return None
                } else {
                    Err(err) // otherwise fail
                }
            }
        }
    }

    fn many<T>(&mut self, func: ParseFunc<T>) -> ParseResult<Vec<T>> {
        let mut vec = Vec::new();
        let mut last = self.cursor;
        loop {
            match func(self) {
                Ok(res) => {
                    vec.push(res);
                    last = self.cursor;
                }
                Err(err) => {
                    // if it failed without consuming any token
                    if self.cursor == last {
                        return Ok(vec); // return the result
                    } else {
                        return Err(err); // rethrow the error
                    }
                }
            }
        }
    }

    fn many1<T>(&mut self, func: ParseFunc<T>) -> ParseResult<Vec<T>> {
        let first = func(self)?;
        let mut vec = self.many(func)?;
        vec.insert(0, first);
        Ok(vec)
    }

    fn sepby<T>(&mut self, delim: TokenKind, func: ParseFunc<T>) -> ParseResult<Vec<T>> {
        let last = self.cursor;
        match self.sepby1(delim, func) {
            Ok(res) => Ok(res),
            Err(err) => {
                // if it failed without consuming any token
                if self.cursor == last {
                    Ok(Vec::new()) // return the result
                } else {
                    Err(err) // rethrow the error
                }
            }
        }
    }

    fn sepby1<T>(&mut self, delim: TokenKind, func: ParseFunc<T>) -> ParseResult<Vec<T>> {
        let mut vec = Vec::new();
        let first = func(self)?;
        vec.push(first);
        while self.peek_first() == delim {
            self.next_token();
            let res = func(self)?;
            vec.push(res);
        }
        Ok(vec)
    }
}

pub fn parse_expr(p: &mut Parser) -> ParseResult<Expr> {
    let expr = parse_expr_no_app(p)?;
    let applys = p.many(|p| {
        let start = p.start_pos();
        p.match_token(TokenKind::LParen)?;
        let args = p.sepby(TokenKind::Comma, parse_expr)?;
        p.match_token(TokenKind::RParen)?;
        let span = Span::new(start, p.end_pos());
        Ok((args, span))
    })?;
    let res = applys.into_iter().fold(expr, |func, (args, span)| {
        let span = Span::merge(func.span(), &span);
        let func = Box::new(func);
        Expr::App { func, args, span }
    });
    Ok(res)
}

fn parse_expr_no_app(p: &mut Parser) -> ParseResult<Expr> {
    let start = p.start_pos();
    match p.peek_first() {
        TokenKind::LitInt | TokenKind::LitReal | TokenKind::LitBool | TokenKind::LitChar => {
            let lit = p.match_lit_val().unwrap();
            let span = Span::new(start, p.end_pos());
            Ok(Expr::Lit { lit, span })
        }
        TokenKind::LowerIdent => {
            let var = p.match_lower_ident().unwrap();
            let span = Span::new(start, p.end_pos());
            Ok(Expr::Var { var, span })
        }
        TokenKind::UpperIdent => {
            let cons = p.match_upper_ident().unwrap();
            let args = if p.peek_first() == TokenKind::LParen {
                p.match_token(TokenKind::LParen).unwrap();
                let args = p.sepby(TokenKind::Comma, parse_expr)?;
                p.match_token(TokenKind::RParen)?;
                args
            } else {
                // abbreviate `Cons()` to `Cons`
                Vec::new()
            };
            let span = Span::new(start, p.end_pos());
            Ok(Expr::Cons { cons, args, span })
        }
        TokenKind::Hash => {
            p.match_token(TokenKind::Hash).unwrap();
            let func = p.match_lower_ident()?.name;
            p.match_token(TokenKind::LParen)?;
            let args = p.sepby(TokenKind::Comma, parse_expr)?;
            p.match_token(TokenKind::RParen)?;
            let span = Span::new(start, p.end_pos());
            Ok(Expr::ExtCall { func, args, span })
        }
        TokenKind::Builtin => {
            let prim = p.match_builtin().unwrap();
            p.match_token(TokenKind::LParen)?;
            let args = p.sepby(TokenKind::Comma, parse_expr)?;
            p.match_token(TokenKind::RParen)?;
            let span = Span::new(start, p.end_pos());
            Ok(Expr::Prim { prim, args, span })
        }
        TokenKind::Fn => {
            p.match_token(TokenKind::Fn).unwrap();
            p.match_token(TokenKind::LParen)?;
            let pars = p.sepby(TokenKind::Comma, |p| p.match_lower_ident())?;
            p.match_token(TokenKind::RParen)?;
            p.match_token(TokenKind::EArrow)?;
            let body = Box::new(parse_expr(p)?);
            let span = Span::new(start, p.end_pos());
            Ok(Expr::Fun { pars, body, span })
        }

        TokenKind::Begin => {
            p.match_token(TokenKind::Begin).unwrap();
            let block = Box::new(parse_block(p)?);
            p.match_token(TokenKind::End)?;
            let span = Span::new(start, p.end_pos());
            Ok(Expr::Begin { block, span })
        }
        TokenKind::Case => {
            p.match_token(TokenKind::Case).unwrap();
            let expr = Box::new(parse_expr(p)?);
            p.match_token(TokenKind::Of)?;
            let rules = p.many1(parse_rule)?;
            p.match_token(TokenKind::End)?;
            let span = Span::new(start, p.end_pos());
            Ok(Expr::Case { expr, rules, span })
        }
        TokenKind::If => {
            p.match_token(TokenKind::If).unwrap();
            let cond = Box::new(parse_expr(p)?);
            p.match_token(TokenKind::Then)?;
            let trbr = Box::new(parse_expr(p)?);
            p.match_token(TokenKind::Else)?;
            let flbr = Box::new(parse_expr(p)?);
            let span = Span::new(start, p.end_pos());
            Ok(Expr::Ifte {
                cond,
                trbr,
                flbr,
                span,
            })
        }
        TokenKind::Letrec => {
            p.match_token(TokenKind::Letrec).unwrap();
            let decls = p.many(parse_decl)?;
            p.match_token(TokenKind::In)?;
            let block = Box::new(parse_block(p)?);
            p.match_token(TokenKind::End)?;
            let span = Span::new(start, p.end_pos());
            Ok(Expr::Letrec { decls, block, span })
        }
        TokenKind::LParen => {
            p.match_token(TokenKind::LParen).unwrap();
            let mut expr = parse_expr(p)?;
            p.match_token(TokenKind::RParen)?;
            *expr.span_mut() = Span::new(start, p.end_pos());
            Ok(expr)
        }
        TokenKind::LBrace => {
            p.match_token(TokenKind::LBrace).unwrap();
            let mut expr = parse_expr(p)?;
            p.match_token(TokenKind::RBrace)?;
            *expr.span_mut() = Span::new(start, p.end_pos());
            Ok(expr)
        }
        _ => {
            static VEC: &[TokenKind] = &[
                TokenKind::LitInt,
                TokenKind::LitReal,
                TokenKind::LitBool,
                TokenKind::LitChar,
                TokenKind::LowerIdent,
                TokenKind::Builtin,
                TokenKind::Fn,
                TokenKind::Let,
                TokenKind::Case,
                TokenKind::Begin,
                TokenKind::LParen,
            ];
            Err(p.err_unexpected_many(VEC))
        }
    }
}

fn parse_block(p: &mut Parser) -> ParseResult<Block> {
    let start = p.start_pos();
    let stmts = p.many(parse_stmt)?;
    let retn = p.option(|p| parse_expr(p))?;
    let span = Span::new(start, p.end_pos());
    Ok(Block { stmts, retn, span })
}

fn parse_stmt(p: &mut Parser) -> ParseResult<Stmt> {
    let rec = p.backup();
    let start = p.start_pos();
    match p.peek_first() {
        TokenKind::Let => {
            p.match_token(TokenKind::Let).unwrap();
            let bind = p.match_lower_ident()?;
            let typ = p.option(|p| {
                p.match_token(TokenKind::Colon)?;
                parse_type(p)
            })?;
            p.match_token(TokenKind::Equal)?;
            let expr = parse_expr(p)?;
            p.match_token(TokenKind::Semi)?;
            let span = Span::new(start, p.end_pos());
            Ok(Stmt::Bind {
                bind,
                typ,
                expr,
                span,
            })
        }
        _ => {
            let expr = parse_expr(p)?;
            p.match_token(TokenKind::Semi).map_err(|err| {
                p.recover(rec);
                err
            })?;
            let span = Span::new(start, p.end_pos());
            Ok(Stmt::Do { expr, span })
        }
    }
}

fn parse_pattern(p: &mut Parser) -> ParseResult<Pattern> {
    let start = p.start_pos();
    match p.peek_first() {
        TokenKind::LitInt | TokenKind::LitReal | TokenKind::LitBool | TokenKind::LitChar => {
            let lit = p.match_lit_val().unwrap();
            let span = Span::new(start, p.end_pos());
            Ok(Pattern::Lit { lit, span })
        }
        TokenKind::LParen if p.peek_second() == TokenKind::RParen => {
            let lit = p.match_lit_val().unwrap();
            let span = Span::new(start, p.end_pos());
            Ok(Pattern::Lit { lit, span })
        }
        TokenKind::LowerIdent => {
            let var = p.match_lower_ident().unwrap();
            let span = Span::new(start, p.end_pos());
            Ok(Pattern::Var { var, span })
        }
        TokenKind::UpperIdent => {
            let cons = p.match_upper_ident().unwrap();
            let pars = if p.peek_first() == TokenKind::LParen {
                p.match_token(TokenKind::LParen).unwrap();
                let pars = p.sepby(TokenKind::Comma, parse_pattern)?;
                p.match_token(TokenKind::RParen)?;
                pars
            } else {
                // abbreviate `| Cons() => ...` to `| Cons => ...`
                Vec::new()
            };
            let span = Span::new(start, p.end_pos());
            Ok(Pattern::Cons { cons, pars, span })
        }
        TokenKind::Wild => {
            let span = *p.peek_span();
            Ok(Pattern::Wild { span })
        }
        _ => {
            static VEC: &[TokenKind] = &[
                TokenKind::LitInt,
                // real numbers couldn't appear in pattern matching!
                // TokenKind::LitReal,
                TokenKind::LitBool,
                TokenKind::LitChar,
                TokenKind::LParen,
                TokenKind::LowerIdent,
                TokenKind::Wild,
            ];
            Err(p.err_unexpected_many(VEC))
        }
    }
}

fn parse_rule(p: &mut Parser) -> ParseResult<Rule> {
    let start = p.start_pos();
    p.match_token(TokenKind::Bar)?;
    let patn = parse_pattern(p)?;
    p.match_token(TokenKind::EArrow)?;
    let body = parse_expr(p)?;
    let span = Span::new(start, p.end_pos());
    Ok(Rule { patn, body, span })
}

pub fn parse_decl(p: &mut Parser) -> ParseResult<Decl> {
    let start = p.start_pos();
    match p.peek_first() {
        TokenKind::Func => {
            p.match_token(TokenKind::Func).unwrap();
            let name = p.match_lower_ident()?;
            let gens = p
                .option(|p| {
                    p.match_token(TokenKind::LBracket)?;
                    let gens = p.many1(|p| p.match_upper_ident())?;
                    p.match_token(TokenKind::RBracket)?;
                    Ok(gens)
                })?
                .unwrap_or(Vec::new());
            p.match_token(TokenKind::LParen)?;
            let pars = p.sepby(TokenKind::Comma, |p| {
                let par = p.match_lower_ident()?;
                p.match_token(TokenKind::Colon)?;
                let typ = parse_type(p)?;
                Ok((par, typ))
            })?;
            p.match_token(TokenKind::RParen)?;
            let res = p
                .option(|p| {
                    p.match_token(TokenKind::Colon)?;
                    parse_type(p)
                })?
                .unwrap_or(Type::Lit {
                    lit: LitType::Unit,
                    span: Span::new(p.start_pos(), p.end_pos()),
                });
            p.match_token(TokenKind::Equal)?;
            let body = Box::new(parse_expr(p)?);
            let span = Span::new(start, p.end_pos());
            Ok(Decl::Func {
                name,
                gens,
                pars,
                res,
                body,
                span,
            })
        }
        TokenKind::Data => {
            p.match_token(TokenKind::Data).unwrap();
            let name = p.match_upper_ident()?;
            let pars = p
                .option(|p| {
                    p.match_token(TokenKind::LBracket)?;
                    let pars = p.sepby1(TokenKind::Comma, |p| p.match_upper_ident())?;
                    p.match_token(TokenKind::RBracket)?;
                    Ok(pars)
                })?
                .unwrap_or(Vec::new());
            p.match_token(TokenKind::Equal)?;
            let vars = p.many1(|p| {
                p.match_token(TokenKind::Bar)?;
                parse_varient(p)
            })?;
            p.match_token(TokenKind::End)?;
            let span = Span::new(start, p.end_pos());
            Ok(Decl::Data {
                name,
                pars,
                vars,
                span,
            })
        }
        TokenKind::Type => {
            p.match_token(TokenKind::Type).unwrap();
            let name = p.match_upper_ident()?;
            let pars = p
                .option(|p| {
                    p.match_token(TokenKind::LBracket)?;
                    let pars = p.sepby1(TokenKind::Comma, |p| p.match_upper_ident())?;
                    p.match_token(TokenKind::RBracket)?;
                    Ok(pars)
                })?
                .unwrap_or(Vec::new());
            p.match_token(TokenKind::Equal)?;
            let typ = parse_type(p)?;
            p.match_token(TokenKind::Semi)?;
            let span = Span::new(start, p.end_pos());
            Ok(Decl::Type {
                name,
                pars,
                typ,
                span,
            })
        }
        TokenKind::Extern => {
            p.match_token(TokenKind::Extern).unwrap();
            let name = p.match_lower_ident()?.name;
            let gens = p
                .option(|p| {
                    p.match_token(TokenKind::LBracket)?;
                    let gens = p.sepby1(TokenKind::Comma, |p| p.match_upper_ident())?;
                    p.match_token(TokenKind::RBracket)?;
                    Ok(gens)
                })?
                .unwrap_or(Vec::new());
            p.match_token(TokenKind::Colon)?;
            let typ = parse_type(p)?;
            p.match_token(TokenKind::Semi)?;
            let span = Span::new(start, p.end_pos());
            Ok(Decl::Extern {
                name,
                gens,
                typ,
                span,
            })
        }
        _ => {
            static VEC: &[TokenKind] = &[
                TokenKind::Func,
                TokenKind::Data,
                TokenKind::Type,
                TokenKind::Extern,
            ];
            Err(p.err_unexpected_many(VEC))
        }
    }
}

pub fn parse_varient(p: &mut Parser) -> ParseResult<Varient> {
    let start = p.start_pos();
    let cons = p.match_upper_ident()?;
    let pars = p
        .option(|p| {
            p.match_token(TokenKind::LParen)?;
            let pars = p.sepby1(TokenKind::Comma, parse_type)?;
            p.match_token(TokenKind::RParen)?;
            Ok(pars)
        })?
        .unwrap_or(Vec::new());
    let span = Span::new(start, p.end_pos());
    Ok(Varient { cons, pars, span })
}

fn parse_type(p: &mut Parser) -> ParseResult<Type> {
    let start = p.start_pos();
    match p.peek_first() {
        TokenKind::TyInt | TokenKind::TyReal | TokenKind::TyBool | TokenKind::TyChar => {
            let lit = p.match_lit_type().unwrap();
            let span = Span::new(start, p.end_pos());
            Ok(Type::Lit { lit, span })
        }
        TokenKind::LParen if p.peek_second() == TokenKind::RParen => {
            let lit = p.match_lit_type().unwrap();
            let span = Span::new(start, p.end_pos());
            Ok(Type::Lit { lit, span })
        }
        TokenKind::UpperIdent => {
            let var = p.match_upper_ident().unwrap();
            if p.peek_first() == TokenKind::LBracket {
                p.match_token(TokenKind::LBracket)?;
                let args = p.sepby1(TokenKind::Comma, parse_type)?;
                p.match_token(TokenKind::RBracket)?;
                let span = Span::new(start, p.end_pos());
                Ok(Type::App {
                    cons: var,
                    args,
                    span,
                })
            } else {
                let span = Span::new(start, p.end_pos());
                Ok(Type::Var { var, span })
            }
        }
        TokenKind::Fn => {
            p.match_token(TokenKind::Fn).unwrap();
            p.match_token(TokenKind::LParen)?;
            let pars = p.sepby(TokenKind::Comma, parse_type)?;
            p.match_token(TokenKind::RParen)?;
            p.match_token(TokenKind::Arrow)?;
            let res = Box::new(parse_type(p)?);
            let span = Span::new(start, p.end_pos());
            Ok(Type::Fun { pars, res, span })
        }
        _ => {
            static VEC: &[TokenKind] = &[
                TokenKind::TyInt,
                TokenKind::TyReal,
                TokenKind::TyBool,
                TokenKind::TyChar,
                TokenKind::UpperIdent,
                TokenKind::Fn,
            ];
            Err(p.err_unexpected_many(VEC))
        }
    }
}

#[test]
fn parser_test() {
    let string = r#"
letrec
    extern print_int: fn(Int) -> ();
    extern prg_exit[T]: fn(Int) -> T;
    type My-Int = Int;
    type Option-Int = Option[Int];
    data Option[T] =
    | Some(T)
    | None
    end
    func add1(x: Int): Int = @iadd(x, 1)
    func add2(x: Int): Int = begin
        #print_int(x);
        let y: Int = @iadd(x,1);
        #print_int(y);
        @iadd(y,1)
    end
    func const-3[T](x: T): Int = begin
        3
    end
    func option-add1(x: Option[Int]): Option[Int] =
        case x of
        | Some(y) => Some(@iadd(x,1))
        | None => None
        end
in
    @isub(add1(42), 1)
end
"#;

    let mut par = Parser::new(string);
    let res = parse_expr(&mut par);
    // println!("{res:?}");
    assert!(res.is_ok());
    // println!("{}", res.unwrap());
}
