use crate::intern::InternStr;
use crate::position::{Span, Spanned};

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum LitVal {
    Int(i64),
    Real(f64),
    Bool(bool),
    Char(char),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Builtin {
    IAdd,
    ISub,
    IMul,
    IDiv,
    IRem,
    INeg,
    RAdd,
    RSub,
    RMul,
    RDiv,
    BAnd,
    BOr,
    BNot,
}

impl Builtin {
    pub fn get_arity(&self) -> usize {
        match self {
            Builtin::IAdd => 2,
            Builtin::ISub => 2,
            Builtin::IMul => 2,
            Builtin::IDiv => 2,
            Builtin::INeg => 1,
            Builtin::IRem => 2,
            Builtin::RAdd => 2,
            Builtin::RSub => 2,
            Builtin::RMul => 2,
            Builtin::RDiv => 2,
            Builtin::BAnd => 2,
            Builtin::BOr => 2,
            Builtin::BNot => 1,
        }
    }   
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<Ident = InternStr> {
    Lit {
        lit : LitVal,
        span: Span,
    },
    Opr {
        op: Builtin,
        args: Vec<Expr>,
        span: Span,
    },
    Var {
        var: Ident,
        span: Span,
    },
    Fun {
        pars: Vec<Ident>,
        body: Box<Expr>,
        span: Span,
    },
    App {
        func: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
    Let {
        bind: Ident,
        expr: Box<Expr>,
        cont: Box<Expr>,
        span: Span,
    },
    Blk {
        decls: Vec<Decl<Ident>>,
        cont: Box<Expr>,
        span: Span,
    },
}

impl Spanned for Expr {
    fn span(&self) -> &Span {
        match self {
            Expr::Lit { span, .. } => span,
            Expr::Opr { span, .. } => span,
            Expr::Var { span, .. } => span,
            Expr::Fun { span, .. } => span,
            Expr::App { span, .. } => span,
            Expr::Let { span, .. } => span,
            Expr::Blk { span, .. } => span,
        }
    }
    fn span_mut(&mut self) -> &mut Span {
        match self {
            Expr::Lit { span, .. } => span,
            Expr::Opr { span, .. } => span,
            Expr::Var { span, .. } => span,
            Expr::Fun { span, .. } => span,
            Expr::App { span, .. } => span,
            Expr::Let { span, .. } => span,
            Expr::Blk { span, .. } => span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl<Ident = InternStr> {
    Func {
        func: Ident,
        pars: Vec<Ident>,
        expr: Box<Expr<Ident>>,
        span: Span,
    },
}

impl Spanned for Decl {
    fn span(&self) -> &Span {
        match self {
            Decl::Func { span, .. } => span,
        }
    }
    fn span_mut(&mut self) -> &mut Span {
        match self {
            Decl::Func { span, .. } => span,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd)]
pub enum LitType {
    Int,
    Real,
    Bool,
    Char,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum MonoType<Ident = InternStr> {
    Lit(LitType),
    Var(Ident),
    Arr(Vec<MonoType>, Box<MonoType>),
    App(Box<MonoType>, Vec<MonoType>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PolyType<Ident = InternStr> {
    para: Vec<Ident>,
    body: MonoType,
}
