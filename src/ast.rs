use crate::intern::{InternStr, Unique};
use crate::position::{Span, Spanned};

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum LitVal {
    Int(i64),
    Real(f64),
    Bool(bool),
    Char(char),
}

impl LitVal {
    pub fn get_lit_type(&self) -> LitType {
        match self {
            LitVal::Int(_) => LitType::Int,
            LitVal::Real(_) => LitType::Real,
            LitVal::Bool(_) => LitType::Bool,
            LitVal::Char(_) => LitType::Char,
        }
    }
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
    Cons {
        cons: Ident,
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
            Expr::Cons { span, .. } => span,
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
            Expr::Cons { span, .. } => span,
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
            Expr::Cons { .. } => true,
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

impl<Ident> Pattern<Ident> {
    pub fn is_wild_or_var(&self) -> bool {
        match self {
            Pattern::Var { .. } | Pattern::Wild { .. } => true,
            _ => false,
        }
    }
}
impl Pattern<Unique> {
    pub fn get_freevars(&self) -> Vec<Unique> {
        let mut stack = vec![self];
        let mut vec = Vec::new();

        while let Some(with) = stack.pop() {
            match with {
                Pattern::Var { var, .. } => {
                    if !vec.contains(var) {
                        vec.push(*var);
                    }
                }
                Pattern::Lit { .. } => {}
                Pattern::Cons { pars, .. } => {
                    stack.extend(pars.into_iter());
                }
                Pattern::Wild { .. } => {}
            }
        }
        vec
    }
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
        typ: Type<Ident>,
        span: Span,
    },
}

impl<Ident> Decl<Ident> {
    pub fn get_name(&self) -> &Ident {
        match self {
            Decl::Func { name, .. } => name,
            Decl::Data { name, .. } => name,
            Decl::Type { name, .. } => name,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Varient<Ident = InternStr> {
    pub cons: Ident,
    pub pars: Vec<Type<Ident>>,
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
pub enum Type<Ident = InternStr> {
    Lit {
        lit: LitType,
        span: Span,
    },
    Var {
        var: Ident,
        span: Span,
    },
    Fun {
        pars: Vec<Type<Ident>>,
        res: Box<Type<Ident>>,
        span: Span,
    },
    App {
        cons: Ident,
        args: Vec<Type<Ident>>,
        span: Span,
    },
}

impl<Ident> Spanned for Type<Ident> {
    fn span(&self) -> &Span {
        match self {
            Type::Lit { span, .. } => span,
            Type::Var { span, .. } => span,
            Type::Fun { span, .. } => span,
            Type::App { span, .. } => span,
        }
    }
    fn span_mut(&mut self) -> &mut Span {
        match self {
            Type::Lit { span, .. } => span,
            Type::Var { span, .. } => span,
            Type::Fun { span, .. } => span,
            Type::App { span, .. } => span,
        }
    }
}
