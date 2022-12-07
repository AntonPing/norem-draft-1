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
        lit: LitVal,
        span: Span,
    },
    Var {
        var: Ident,
        span: Span,
    },
    Prim {
        prim: Builtin,
        args: Vec<Expr<Ident>>,
        span: Span,
    },
    Fun {
        pars: Vec<Ident>,
        body: Box<Expr<Ident>>,
        span: Span,
    },
    App {
        func: Box<Expr<Ident>>,
        args: Vec<Expr<Ident>>,
        span: Span,
    },
    Let {
        bind: Ident,
        expr: Box<Expr<Ident>>,
        cont: Box<Expr<Ident>>,
        span: Span,
    },
    Case {
        expr: Box<Expr<Ident>>,
        rules: Vec<Rule<Ident>>,
        span: Span,
    },
    Blk {
        decls: Vec<Decl<Ident>>,
        cont: Box<Expr<Ident>>,
        span: Span,
    },
}

impl<Ident> Spanned for Expr<Ident> {
    fn span(&self) -> &Span {
        match self {
            Expr::Lit { span, .. } => span,
            Expr::Var { span, .. } => span,
            Expr::Prim { span, .. } => span,
            Expr::Fun { span, .. } => span,
            Expr::App { span, .. } => span,
            Expr::Let { span, .. } => span,
            Expr::Case { span, .. } => span,
            Expr::Blk { span, .. } => span,
        }
    }
    fn span_mut(&mut self) -> &mut Span {
        match self {
            Expr::Lit { span, .. } => span,
            Expr::Var { span, .. } => span,
            Expr::Prim { span, .. } => span,
            Expr::Fun { span, .. } => span,
            Expr::App { span, .. } => span,
            Expr::Let { span, .. } => span,
            Expr::Case { span, .. } => span,
            Expr::Blk { span, .. } => span,
        }
    }
}

impl<Ident> Expr<Ident> {
    pub fn is_simple(&self) -> bool {
        match self {
            Expr::Lit { .. } => true,
            Expr::Var { .. } => true,
            Expr::Prim { .. } => true,
            Expr::Fun { .. } => true,
            Expr::App { .. } => true,
            Expr::Let { .. } => false,
            Expr::Case { .. } => false,
            Expr::Blk { .. } => false,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Rule<Ident = InternStr> {
    pub patn: Pattern<Ident>,
    pub body: Expr<Ident>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern<Ident = InternStr> {
    Var {
        var: Ident,
        span: Span,
    },
    Lit {
        lit: LitVal,
        span: Span,
    },
    Cons {
        cons: Ident,
        pars: Vec<Pattern<Ident>>,
        span: Span,
    },
    Wild {
        span: Span,
    },
}

impl<Ident> Spanned for Pattern<Ident> {
    fn span(&self) -> &Span {
        match self {
            Pattern::Var { span, .. } => span,
            Pattern::Lit { span, .. } => span,
            Pattern::Cons { span, .. } => span,
            Pattern::Wild { span } => span,
        }
    }
    fn span_mut(&mut self) -> &mut Span {
        match self {
            Pattern::Var { span, .. } => span,
            Pattern::Lit { span, .. } => span,
            Pattern::Cons { span, .. } => span,
            Pattern::Wild { span } => span,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl<Ident = InternStr> {
    Func {
        name: Ident,
        pars: Vec<Ident>,
        body: Box<Expr<Ident>>,
        span: Span,
    },
    Data {
        name: Ident,
        pars: Vec<Ident>,
        vars: Vec<Varient<Ident>>,
        span: Span,
    },
    Type {
        name: Ident,
        pars: Vec<Ident>,
        typ: MonoType<Ident>,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Varient<Ident = InternStr> {
    pub cons: Ident,
    pub pars: Vec<MonoType<Ident>>,
    pub span: Span,
}

impl<Ident> Spanned for Decl<Ident> {
    fn span(&self) -> &Span {
        match self {
            Decl::Func { span, .. } => span,
            Decl::Data { span, .. } => span,
            Decl::Type { span, .. } => span,
        }
    }
    fn span_mut(&mut self) -> &mut Span {
        match self {
            Decl::Func { span, .. } => span,
            Decl::Data { span, .. } => span,
            Decl::Type { span, .. } => span,
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
    Fun(Vec<MonoType<Ident>>, Box<MonoType<Ident>>),
    App(Box<MonoType<Ident>>, Vec<MonoType<Ident>>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PolyType<Ident = InternStr> {
    pars: Vec<Ident>,
    body: MonoType,
}
