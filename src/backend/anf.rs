use super::*;
use crate::frontend::ast::LitVal;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Atom {
    Var(Ident),
    Int(i64),
    Real(f64),
    Bool(bool),
    Char(char),
    Unit,
}

impl From<LitVal> for Atom {
    fn from(lit: LitVal) -> Self {
        match lit {
            LitVal::Int(x) => Atom::Int(x),
            LitVal::Real(x) => Atom::Real(x),
            LitVal::Bool(x) => Atom::Bool(x),
            LitVal::Char(x) => Atom::Char(x),
            LitVal::Unit => Atom::Unit,
        }
    }
}

impl Atom {
    pub fn is_lit(&self) -> bool {
        match self {
            Atom::Int(_) => true,
            Atom::Real(_) => true,
            Atom::Bool(_) => true,
            Atom::Char(_) => true,
            Atom::Unit => true,
            _ => false,
        }
    }
    pub fn is_var(&self) -> bool {
        match self {
            Atom::Var(_) => true,
            _ => false,
        }
    }
    pub fn unwrap_lit(self) -> LitVal {
        match self {
            Atom::Int(x) => LitVal::Int(x),
            Atom::Real(x) => LitVal::Real(x),
            Atom::Bool(x) => LitVal::Bool(x),
            Atom::Char(x) => LitVal::Char(x),
            _ => panic!("failed to unwrap literal!"),
        }
    }
    pub fn unwrap_var(self) -> Ident {
        match self {
            Atom::Var(x) => x,
            _ => panic!("failed to unwrap variable!"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum UnOpPrim {
    Move,
    INeg,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinOpPrim {
    IAdd,
    ISub,
    IMul,
    ICmpEq,
    ICmpNe,
    ICmpGr,
    ICmpGe,
    ICmpLs,
    ICmpLe,
}

#[derive(Clone, Debug)]
pub enum MExpr {
    LetIn {
        decls: Vec<MDecl>,
        cont: Box<MExpr>,
    },
    UnOp {
        bind: Ident,
        prim: UnOpPrim,
        arg1: Atom,
        cont: Box<MExpr>,
    },
    BinOp {
        bind: Ident,
        prim: BinOpPrim,
        arg1: Atom,
        arg2: Atom,
        cont: Box<MExpr>,
    },
    Call {
        bind: Ident,
        func: Atom,
        args: Vec<Atom>,
        cont: Box<MExpr>,
    },
    ExtCall {
        bind: Ident,
        func: InternStr,
        args: Vec<Atom>,
        cont: Box<MExpr>,
    },
    Retn {
        arg1: Atom,
    },
    Alloc {
        bind: Ident,
        size: usize,
        cont: Box<MExpr>,
    },
    Load {
        bind: Ident,
        arg1: Atom,
        index: usize,
        cont: Box<MExpr>,
    },
    Store {
        arg1: Atom,
        index: usize,
        arg2: Atom,
        cont: Box<MExpr>,
    },
    Offset {
        bind: Ident,
        arg1: Atom,
        index: isize,
        cont: Box<MExpr>,
    },
    Ifte {
        bind: Ident,
        arg1: Atom,
        brch1: Box<MExpr>,
        brch2: Box<MExpr>,
        cont: Box<MExpr>,
    },
    Switch {
        bind: Ident,
        arg1: Atom,
        brchs: Vec<(usize, MExpr)>,
        dflt: Option<Box<MExpr>>,
        cont: Box<MExpr>,
    },
}

impl MExpr {
    pub fn is_retn(&self) -> bool {
        match self {
            MExpr::Retn { .. } => true,
            _ => false,
        }
    }

    pub fn is_tail_call(&self) -> bool {
        if let MExpr::Call { cont, .. } = self {
            cont.is_retn()
        } else {
            false
        }
    }

    pub fn make_tail_call(func: Ident, args: Vec<Atom>) -> MExpr {
        let r = Ident::generate('r');
        MExpr::Call {
            bind: r,
            func: Atom::Var(func),
            args,
            cont: Box::new(MExpr::Retn { arg1: Atom::Var(r) }),
        }
    }
}

#[derive(Clone, Debug)]
pub struct MDecl {
    pub func: Ident,
    pub pars: Vec<Ident>,
    pub body: MExpr,
}
