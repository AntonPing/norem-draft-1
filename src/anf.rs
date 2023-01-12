use crate::ast::LitVal;
use crate::intern::{InternStr, Unique};
use std::collections::HashMap;
use std::fmt::{self, Display};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Atom {
    Var(Unique),
    Int(i64),
    Real(f64),
    Bool(bool),
    Char(char),
}

impl From<LitVal> for Atom {
    fn from(lit: LitVal) -> Self {
        match lit {
            LitVal::Int(x) => Atom::Int(x),
            LitVal::Real(x) => Atom::Real(x),
            LitVal::Bool(x) => Atom::Bool(x),
            LitVal::Char(x) => Atom::Char(x),
        }
    }
}

impl From<Unique> for Atom {
    fn from(var: Unique) -> Self {
        Atom::Var(var)
    }
}

impl From<i64> for Atom {
    fn from(x: i64) -> Self {
        Atom::Int(x)
    }
}

impl From<f64> for Atom {
    fn from(x: f64) -> Self {
        Atom::Real(x)
    }
}

impl From<bool> for Atom {
    fn from(x: bool) -> Self {
        Atom::Bool(x)
    }
}

impl From<char> for Atom {
    fn from(x: char) -> Self {
        Atom::Char(x)
    }
}

impl Atom {
    pub fn is_literal(&self) -> bool {
        match self {
            Atom::Int(_) => true,
            Atom::Real(_) => true,
            Atom::Bool(_) => true,
            Atom::Char(_) => true,
            _ => false,
        }
    }
    pub fn is_variable(&self) -> bool {
        match self {
            Atom::Var(_) => true,
            _ => false,
        }
    }
    pub fn unwrap_literal(self) -> LitVal {
        match self {
            Atom::Int(x) => LitVal::Int(x),
            Atom::Real(x) => LitVal::Real(x),
            Atom::Bool(x) => LitVal::Bool(x),
            Atom::Char(x) => LitVal::Char(x),
            _ => panic!("failed to unwrap literal!"),
        }
    }
    pub fn unwrap_variable(self) -> Unique {
        match self {
            Atom::Var(x) => x,
            _ => panic!("failed to unwrap variable!"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum MStmt {
    IAdd {
        arg1: Atom,
        arg2: Atom,
    },
    ISub {
        arg1: Atom,
        arg2: Atom,
    },
    IMul {
        arg1: Atom,
        arg2: Atom,
    },
    Move {
        arg1: Atom,
    },
    Alloc {
        size: usize,
    },
    Load {
        arg1: Atom,
        index: usize,
    },
    Store {
        arg1: Atom,
        index: usize,
        arg2: Atom,
    },
    Offset {
        arg1: Atom,
        index: usize,
    },
    Ifte {
        arg1: Atom,
        brch1: Box<MExpr>,
        brch2: Box<MExpr>,
    },
    Switch {
        arg1: Atom,
        brchs: Vec<MExpr>,
    },
}

impl MStmt {
    pub fn is_pure(&self) -> bool {
        match self {
            MStmt::IAdd { .. } | MStmt::ISub { .. } | MStmt::IMul { .. } | MStmt::Move { .. } => {
                true
            }
            MStmt::Alloc { .. }
            | MStmt::Load { .. }
            | MStmt::Store { .. }
            | MStmt::Offset { .. }
            | MStmt::Ifte { .. }
            | MStmt::Switch { .. } => false,
        }
    }
    pub fn is_branch(&self) -> bool {
        match self {
            MStmt::Ifte { .. } | MStmt::Switch { .. } => true,
            _ => false,
        }
    }
}

impl<'a> MStmt {
    pub fn args_map<F>(self, mut func: F) -> Self
    where
        F: FnMut(Atom) -> Atom,
    {
        match self {
            MStmt::IAdd { arg1, arg2 } => {
                let arg1 = func(arg1);
                let arg2 = func(arg2);
                MStmt::IAdd { arg1, arg2 }
            }
            MStmt::ISub { arg1, arg2 } => {
                let arg1 = func(arg1);
                let arg2 = func(arg2);
                MStmt::ISub { arg1, arg2 }
            }
            MStmt::IMul { arg1, arg2 } => {
                let arg1 = func(arg1);
                let arg2 = func(arg2);
                MStmt::IMul { arg1, arg2 }
            }
            MStmt::Move { arg1 } => {
                let arg1 = func(arg1);
                MStmt::Move { arg1 }
            }
            MStmt::Alloc { size } => MStmt::Alloc { size },
            MStmt::Load { arg1, index } => {
                let arg1 = func(arg1);
                MStmt::Load { arg1, index }
            }
            MStmt::Store { arg1, index, arg2 } => {
                let arg1 = func(arg1);
                let arg2 = func(arg2);
                MStmt::Store { arg1, index, arg2 }
            }
            MStmt::Offset { arg1, index } => {
                let arg1 = func(arg1);
                MStmt::Offset { arg1, index }
            }
            MStmt::Ifte { arg1, brch1, brch2 } => {
                let arg1 = func(arg1);
                MStmt::Ifte { arg1, brch1, brch2 }
            }
            MStmt::Switch { arg1, brchs } => {
                let arg1 = func(arg1);
                MStmt::Switch { arg1, brchs }
            }
        }
    }

    pub fn brchs_map<F>(self, mut func: F) -> Self
    where
        F: FnMut(MExpr) -> MExpr,
    {
        match self {
            MStmt::Ifte { arg1, brch1, brch2 } => {
                let brch1 = Box::new(func(*brch1));
                let brch2 = Box::new(func(*brch2));
                MStmt::Ifte { arg1, brch1, brch2 }
            }
            MStmt::Switch { arg1, brchs } => {
                let brchs = brchs.into_iter().map(|brch| func(brch)).collect();
                MStmt::Switch { arg1, brchs }
            }
            _ => self,
        }
    }

    pub fn args_for_each<F>(&self, mut func: F)
    where
        F: FnMut(&Atom),
    {
        match self {
            MStmt::IAdd { arg1, arg2 } => {
                func(arg1);
                func(arg2);
            }
            MStmt::ISub { arg1, arg2 } => {
                func(arg1);
                func(arg2);
            }
            MStmt::IMul { arg1, arg2 } => {
                func(arg1);
                func(arg2);
            }
            MStmt::Move { arg1 } => {
                func(arg1);
            }
            MStmt::Alloc { .. } => {}
            MStmt::Load { arg1, .. } => {
                func(arg1);
            }
            MStmt::Store { arg1, arg2, .. } => {
                func(arg1);
                func(arg2);
            }
            MStmt::Offset { arg1, .. } => {
                func(arg1);
            }
            MStmt::Ifte { arg1, .. } => {
                func(arg1);
            }
            MStmt::Switch { arg1, .. } => {
                func(arg1);
            }
        }
    }

    pub fn brchs_for_each<F>(&self, mut func: F)
    where
        F: FnMut(&MExpr),
    {
        match self {
            MStmt::Ifte { brch1, brch2, .. } => {
                func(brch1);
                func(brch2);
            }
            MStmt::Switch { brchs, .. } => {
                for brch in brchs {
                    func(brch);
                }
            }
            _ => {}
        }
    }

    pub fn args_mut_for_each<F>(&mut self, mut func: F)
    where
        F: FnMut(&mut Atom),
    {
        match self {
            MStmt::IAdd { arg1, arg2 } => {
                func(arg1);
                func(arg2);
            }
            MStmt::ISub { arg1, arg2 } => {
                func(arg1);
                func(arg2);
            }
            MStmt::IMul { arg1, arg2 } => {
                func(arg1);
                func(arg2);
            }
            MStmt::Move { arg1 } => {
                func(arg1);
            }
            MStmt::Alloc { .. } => {}
            MStmt::Load { arg1, .. } => {
                func(arg1);
            }
            MStmt::Store { arg1, arg2, .. } => {
                func(arg1);
                func(arg2);
            }
            MStmt::Offset { arg1, .. } => {
                func(arg1);
            }
            MStmt::Ifte { arg1, .. } => {
                func(arg1);
            }
            MStmt::Switch { arg1, .. } => {
                func(arg1);
            }
        }
    }

    pub fn brchs_mut_for_each<F>(&mut self, mut func: F)
    where
        F: FnMut(&mut MExpr),
    {
        match self {
            MStmt::Ifte { brch1, brch2, .. } => {
                func(brch1);
                func(brch2);
            }
            MStmt::Switch { brchs, .. } => {
                for brch in brchs {
                    func(brch);
                }
            }
            _ => {}
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CallFunc {
    Intern(Unique),
    Extern(InternStr),
}

#[derive(Clone, Debug)]
pub enum MExpr {
    LetIn {
        decls: Vec<MDecl>,
        cont: Box<MExpr>,
    },
    Stmt {
        bind: Option<Unique>,
        stmt: MStmt,
        cont: Box<MExpr>,
    },
    Call {
        bind: Option<Unique>,
        func: CallFunc,
        args: Vec<Atom>,
        cont: Box<MExpr>,
    },
    Retn {
        atom: Atom,
    },
}

impl MExpr {
    pub fn is_retn(&self) -> bool {
        if let MExpr::Retn { .. } = self {
            true
        } else {
            false
        }
    }

    pub fn is_tail_call(&self) -> bool {
        if let MExpr::Call { cont, .. } = self {
            cont.is_retn()
        } else {
            false
        }
    }

    pub fn make_tail_call(func: Unique, args: Vec<Atom>) -> MExpr {
        let r = Unique::generate('r');
        MExpr::Call {
            bind: Some(r),
            func: CallFunc::Intern(func),
            args,
            cont: Box::new(MExpr::Retn { atom: Atom::Var(r) }),
        }
    }

    pub fn concat(self, bind: Option<Unique>, other: MExpr) -> MExpr {
        match self {
            MExpr::LetIn { decls, cont } => {
                let cont = Box::new(cont.concat(bind, other));
                MExpr::LetIn { decls, cont }
            }
            MExpr::Stmt { bind, stmt, cont } => {
                let cont = Box::new(cont.concat(bind, other));
                MExpr::Stmt { bind, stmt, cont }
            }
            MExpr::Call {
                bind,
                func,
                args,
                cont,
            } => {
                let cont = Box::new(cont.concat(bind, other));
                MExpr::Call {
                    bind,
                    func,
                    args,
                    cont,
                }
            }
            MExpr::Retn { atom } => {
                if bind.is_some() {
                    MExpr::Stmt {
                        bind,
                        stmt: MStmt::Move { arg1: atom },
                        cont: Box::new(other),
                    }
                } else {
                    other
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct MDecl {
    pub func: Unique,
    pub pars: Vec<Unique>,
    pub body: MExpr,
}

impl Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::Var(x) => write!(f, "{x}"),
            Atom::Int(x) => write!(f, "{x}"),
            Atom::Real(x) => write!(f, "{x}"),
            Atom::Bool(x) => write!(f, "{x}"),
            Atom::Char(x) => write!(f, "{x}"),
        }
    }
}

impl Display for CallFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CallFunc::Intern(func) => write!(f, "{func}"),
            CallFunc::Extern(func) => write!(f, "{func}"),
        }
    }
}

struct AlphaEquiv {
    map: HashMap<Unique, Unique>,
}

impl AlphaEquiv {
    fn new() -> AlphaEquiv {
        AlphaEquiv {
            map: HashMap::new(),
        }
    }

    fn eq_ident(&mut self, ident1: &Unique, ident2: &Unique) -> bool {
        // println!("unify {ident1} and {ident2}");
        if let Some(bind) = self.map.get(&ident1) {
            bind == ident2
        } else {
            self.map.insert(*ident1, *ident2);
            true
        }
    }

    fn eq_atom(&mut self, atom1: &Atom, atom2: &Atom) -> bool {
        match (atom1, atom2) {
            (Atom::Var(var1), Atom::Var(var2)) => self.eq_ident(var1, var2),
            (atom1, atom2) => atom1 == atom2,
        }
    }

    fn eq_bind(&mut self, bind1: &Option<Unique>, bind2: &Option<Unique>) -> bool {
        match (bind1, bind2) {
            (Some(bind1), Some(bind2)) => self.eq_ident(bind1, bind2),
            (None, None) => true,
            (_, _) => false,
        }
    }

    fn eq_call_func(&mut self, func1: &CallFunc, func2: &CallFunc) -> bool {
        match (func1, func2) {
            (CallFunc::Intern(ident1), CallFunc::Intern(ident2)) => self.eq_ident(ident1, ident2),
            (CallFunc::Extern(s1), CallFunc::Extern(s2)) => s1 == s2,
            _ => false,
        }
    }

    fn eq_decl(&mut self, decl1: &MDecl, decl2: &MDecl) -> bool {
        let MDecl {
            func: func1,
            pars: pars1,
            body: body1,
        } = decl1;
        let MDecl {
            func: func2,
            pars: pars2,
            body: body2,
        } = decl2;

        self.eq_ident(func1, func2)
            && pars1.len() == pars2.len()
            && pars1
                .iter()
                .zip(pars2.iter())
                .all(|(par1, par2)| self.eq_ident(par1, par2))
            && self.eq_expr(body1, body2)
    }

    fn eq_expr(&mut self, expr1: &MExpr, expr2: &MExpr) -> bool {
        match (expr1, expr2) {
            (
                MExpr::LetIn {
                    decls: decls1,
                    cont: cont1,
                },
                MExpr::LetIn {
                    decls: decls2,
                    cont: cont2,
                },
            ) => {
                decls1.len() == decls2.len()
                    && decls1
                        .iter()
                        .zip(decls2.iter())
                        .all(|(decl1, decl2)| self.eq_decl(decl1, decl2))
                    && self.eq_expr(cont1, cont2)
            }
            (
                MExpr::Stmt {
                    bind: bind1,
                    stmt: stmt1,
                    cont: cont1,
                },
                MExpr::Stmt {
                    bind: bind2,
                    stmt: stmt2,
                    cont: cont2,
                },
            ) => {
                self.eq_bind(bind1, bind2)
                    && match (stmt1, stmt2) {
                        (
                            MStmt::IAdd {
                                arg1: arg11,
                                arg2: arg12,
                            },
                            MStmt::IAdd {
                                arg1: arg21,
                                arg2: arg22,
                            },
                        )
                        | (
                            MStmt::ISub {
                                arg1: arg11,
                                arg2: arg12,
                            },
                            MStmt::ISub {
                                arg1: arg21,
                                arg2: arg22,
                            },
                        )
                        | (
                            MStmt::IMul {
                                arg1: arg11,
                                arg2: arg12,
                            },
                            MStmt::IMul {
                                arg1: arg21,
                                arg2: arg22,
                            },
                        ) => self.eq_atom(arg11, arg21) && self.eq_atom(arg12, arg22),
                        (MStmt::Move { arg1: arg11 }, MStmt::Move { arg1: arg21 }) => {
                            self.eq_atom(arg11, arg21)
                        }
                        (MStmt::Alloc { size: size1 }, MStmt::Alloc { size: size2 }) => {
                            size1 == size2
                        }
                        (
                            MStmt::Load {
                                arg1: arg11,
                                index: index1,
                            },
                            MStmt::Load {
                                arg1: arg21,
                                index: index2,
                            },
                        ) => self.eq_atom(arg11, arg21) && index1 == index2,
                        (
                            MStmt::Store {
                                arg1: arg11,
                                index: index1,
                                arg2: arg12,
                            },
                            MStmt::Store {
                                arg1: arg21,
                                index: index2,
                                arg2: arg22,
                            },
                        ) => {
                            self.eq_atom(arg11, arg21)
                                && index1 == index2
                                && self.eq_atom(arg12, arg22)
                        }
                        (
                            MStmt::Offset {
                                arg1: arg11,
                                index: index1,
                            },
                            MStmt::Offset {
                                arg1: arg21,
                                index: index2,
                            },
                        ) => self.eq_atom(arg11, arg21) && index1 == index2,
                        (
                            MStmt::Ifte {
                                arg1: arg11,
                                brch1: brch11,
                                brch2: brch12,
                            },
                            MStmt::Ifte {
                                arg1: arg21,
                                brch1: brch21,
                                brch2: brch22,
                            },
                        ) => {
                            self.eq_atom(arg11, arg21)
                                && self.eq_expr(brch11, brch12)
                                && self.eq_expr(brch21, brch22)
                        }
                        (
                            MStmt::Switch {
                                arg1: arg11,
                                brchs: brchs1,
                            },
                            MStmt::Switch {
                                arg1: arg21,
                                brchs: brchs2,
                            },
                        ) => {
                            self.eq_atom(arg11, arg21)
                                && brchs1
                                    .iter()
                                    .zip(brchs2.iter())
                                    .all(|(brch1, brch2)| self.eq_expr(brch1, brch2))
                        }
                        (_, _) => false,
                    }
                    && self.eq_expr(cont1, cont2)
            }
            (
                MExpr::Call {
                    bind: bind1,
                    func: func1,
                    args: args1,
                    cont: cont1,
                },
                MExpr::Call {
                    bind: bind2,
                    func: func2,
                    args: args2,
                    cont: cont2,
                },
            ) => {
                self.eq_bind(bind1, bind2)
                    && self.eq_call_func(func1, func2)
                    && args1.len() == args2.len()
                    && args1
                        .iter()
                        .zip(args2.iter())
                        .all(|(arg1, arg2)| self.eq_atom(arg1, arg2))
                    && self.eq_expr(cont1, cont2)
            }
            (MExpr::Retn { atom: atom1 }, MExpr::Retn { atom: atom2 }) => {
                self.eq_atom(atom1, atom2)
            }
            (_, _) => false,
        }
    }
}

impl PartialEq for MExpr {
    fn eq(&self, other: &Self) -> bool {
        // use alpha-equivalence instead of identical comparison
        let mut pass = AlphaEquiv::new();
        pass.eq_expr(self, other)
    }
}

impl PartialEq for MDecl {
    fn eq(&self, other: &Self) -> bool {
        // use alpha-equivalence instead of identical comparison
        let mut pass = AlphaEquiv::new();
        pass.eq_decl(self, other)
    }
}

pub mod anf_build {
    use super::*;
    use crate::intern::intern;
    pub fn i(x: i64) -> Atom {
        Atom::Int(x)
    }
    pub fn r(x: f64) -> Atom {
        Atom::Real(x)
    }
    pub fn b(x: bool) -> Atom {
        Atom::Bool(x)
    }
    pub fn c(x: char) -> Atom {
        Atom::Char(x)
    }
    pub fn v(x: &str) -> Atom {
        Atom::Var(intern(x).as_dummy())
    }
    pub fn u(x: &str) -> Unique {
        intern(x).as_dummy()
    }
    pub fn fun(func: &str, pars: Vec<&str>, body: MExpr) -> MDecl {
        MDecl {
            func: u(func),
            pars: pars.into_iter().map(|par| u(par)).collect(),
            body,
        }
    }
    pub fn block(vec: Vec<MExpr>) -> MExpr {
        vec.into_iter()
            .rev()
            .reduce(|e1, e2| match e2 {
                MExpr::LetIn { decls, cont } => {
                    assert!(cont.is_retn());
                    MExpr::LetIn {
                        decls,
                        cont: Box::new(e1),
                    }
                }
                MExpr::Stmt { bind, stmt, cont } => {
                    assert!(cont.is_retn());
                    MExpr::Stmt {
                        bind,
                        stmt,
                        cont: Box::new(e1),
                    }
                }
                MExpr::Call {
                    bind,
                    func,
                    args,
                    cont,
                } => {
                    assert!(cont.is_retn());
                    MExpr::Call {
                        bind,
                        func,
                        args,
                        cont: Box::new(e1),
                    }
                }
                MExpr::Retn { atom } => MExpr::Retn { atom },
            })
            .unwrap()
    }
    pub fn letin_block(decls: Vec<MDecl>, vec: Vec<MExpr>) -> MExpr {
        MExpr::LetIn {
            decls,
            cont: Box::new(block(vec)),
        }
    }
    pub fn iadd(bind: &str, arg1: Atom, arg2: Atom) -> MExpr {
        MExpr::Stmt {
            bind: if bind == "_" { None } else { Some(u(bind)) },
            stmt: MStmt::IAdd { arg1, arg2 },
            cont: Box::new(MExpr::Retn {
                atom: Atom::Var(u(bind)),
            }),
        }
    }
    pub fn isub(bind: &str, arg1: Atom, arg2: Atom) -> MExpr {
        MExpr::Stmt {
            bind: if bind == "_" { None } else { Some(u(bind)) },
            stmt: MStmt::ISub { arg1, arg2 },
            cont: Box::new(MExpr::Retn {
                atom: Atom::Var(u(bind)),
            }),
        }
    }
    pub fn imul(bind: &str, arg1: Atom, arg2: Atom) -> MExpr {
        MExpr::Stmt {
            bind: if bind == "_" { None } else { Some(u(bind)) },
            stmt: MStmt::IMul { arg1, arg2 },
            cont: Box::new(MExpr::Retn {
                atom: Atom::Var(u(bind)),
            }),
        }
    }
    // well, `move` is rust keyword!
    pub fn _move(bind: &str, arg1: Atom) -> MExpr {
        MExpr::Stmt {
            bind: if bind == "_" { None } else { Some(u(bind)) },
            stmt: MStmt::Move { arg1 },
            cont: Box::new(MExpr::Retn {
                atom: Atom::Var(u(bind)),
            }),
        }
    }
    pub fn alloc(bind: &str, size: usize) -> MExpr {
        MExpr::Stmt {
            bind: if bind == "_" { None } else { Some(u(bind)) },
            stmt: MStmt::Alloc { size },
            cont: Box::new(MExpr::Retn {
                atom: Atom::Var(u(bind)),
            }),
        }
    }
    pub fn load(bind: &str, arg1: Atom, index: usize) -> MExpr {
        MExpr::Stmt {
            bind: if bind == "_" { None } else { Some(u(bind)) },
            stmt: MStmt::Load { arg1, index },
            cont: Box::new(MExpr::Retn {
                atom: Atom::Var(u(bind)),
            }),
        }
    }
    pub fn store(arg1: Atom, index: usize, arg2: Atom) -> MExpr {
        MExpr::Stmt {
            bind: None,
            stmt: MStmt::Store { arg1, index, arg2 },
            cont: Box::new(MExpr::Retn {
                atom: Atom::Var(u("_")),
            }),
        }
    }
    pub fn offset(bind: &str, arg1: Atom, index: usize) -> MExpr {
        MExpr::Stmt {
            bind: if bind == "_" { None } else { Some(u(bind)) },
            stmt: MStmt::Offset { arg1, index },
            cont: Box::new(MExpr::Retn {
                atom: Atom::Var(u(bind)),
            }),
        }
    }
    pub fn ifte(bind: &str, arg1: Atom, brch1: MExpr, brch2: MExpr) -> MExpr {
        MExpr::Stmt {
            bind: if bind == "_" { None } else { Some(u(bind)) },
            stmt: MStmt::Ifte {
                arg1,
                brch1: Box::new(brch1),
                brch2: Box::new(brch2),
            },
            cont: Box::new(MExpr::Retn {
                atom: Atom::Var(u(bind)),
            }),
        }
    }
    pub fn switch(bind: &str, arg1: Atom, brchs: Vec<MExpr>) -> MExpr {
        MExpr::Stmt {
            bind: if bind == "_" { None } else { Some(u(bind)) },
            stmt: MStmt::Switch { arg1, brchs },
            cont: Box::new(MExpr::Retn {
                atom: Atom::Var(u(bind)),
            }),
        }
    }
    pub fn call(bind: &str, func: &str, args: Vec<Atom>) -> MExpr {
        MExpr::Call {
            bind: if bind == "_" { None } else { Some(u(bind)) },
            func: CallFunc::Intern(u(func)),
            args,
            cont: Box::new(MExpr::Retn {
                atom: Atom::Var(u(bind)),
            }),
        }
    }
    pub fn call_ext(bind: &str, func: &str, args: Vec<Atom>) -> MExpr {
        MExpr::Call {
            bind: if bind == "_" { None } else { Some(u(bind)) },
            func: CallFunc::Extern(intern(func)),
            args,
            cont: Box::new(MExpr::Retn {
                atom: Atom::Var(u(bind)),
            }),
        }
    }
    pub fn retn(atom: Atom) -> MExpr {
        MExpr::Retn { atom }
    }
}

#[test]
#[ignore]
fn anf_build_test() {
    use self::anf_build::*;
    let expr = block(vec![
        iadd("a", i(42), i(1)),
        iadd("b", v("a"), v("a")),
        ifte(
            "c",
            v("a"),
            block(vec![iadd("c", v("a"), v("a"))]),
            retn(v("a")),
        ),
        retn(v("c")),
    ]);
    println!("{expr}")
}

#[test]
fn alpha_equiv_test() {
    use self::anf_build::*;
    let expr1 = block(vec![
        iadd("x", i(1), i(2)),
        imul("y", v("x"), v("x")),
        retn(v("y")),
    ]);

    let expr2 = block(vec![
        iadd("a", i(1), i(2)),
        imul("b", v("a"), v("a")),
        retn(v("b")),
    ]);

    let expr3 = block(vec![
        iadd("a", i(1), i(2)),
        imul("b", v("a"), v("a")),
        retn(v("a")),
    ]);

    assert_eq!(expr1, expr2);
    assert_ne!(expr1, expr3);
    assert_ne!(expr2, expr3);
}
