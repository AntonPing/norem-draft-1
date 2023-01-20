use crate::ast::LitVal;
use crate::intern::Unique;
use std::collections::HashMap;
use std::fmt::Debug;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Atom {
    Var(Unique),
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
    pub fn unwrap_var(self) -> Unique {
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
}

#[derive(Clone, Debug)]
pub enum MExpr {
    LetIn {
        decls: Vec<MDecl>,
        cont: Box<MExpr>,
    },
    UnOp {
        bind: Unique,
        prim: UnOpPrim,
        arg1: Atom,
        cont: Box<MExpr>,
    },
    BinOp {
        bind: Unique,
        prim: BinOpPrim,
        arg1: Atom,
        arg2: Atom,
        cont: Box<MExpr>,
    },
    Call {
        bind: Unique,
        func: Atom,
        args: Vec<Atom>,
        cont: Box<MExpr>,
    },
    Retn {
        arg1: Atom,
    },
    Alloc {
        bind: Unique,
        size: usize,
        cont: Box<MExpr>,
    },
    Load {
        bind: Unique,
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
        bind: Unique,
        arg1: Atom,
        index: usize,
        cont: Box<MExpr>,
    },
    Ifte {
        bind: Unique,
        arg1: Atom,
        brch1: Box<MExpr>,
        brch2: Box<MExpr>,
        cont: Box<MExpr>,
    },
    Switch {
        bind: Unique,
        arg1: Atom,
        brchs: Vec<MExpr>,
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

    pub fn make_tail_call(func: Unique, args: Vec<Atom>) -> MExpr {
        let r = Unique::generate('r');
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
    pub func: Unique,
    pub pars: Vec<Unique>,
    pub body: MExpr,
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

    fn eq_decl(&mut self, decl1: &MDecl, decl2: &MDecl) -> bool {
        let MDecl { func, pars, body } = decl1;
        let MDecl {
            func: func_,
            pars: pars_,
            body: body_,
        } = decl2;

        self.eq_ident(func, func_)
            && pars.len() == pars_.len()
            && pars
                .iter()
                .zip(pars_.iter())
                .all(|(par, par_)| self.eq_ident(par, par_))
            && self.eq_expr(body, body_)
    }

    fn eq_expr(&mut self, expr1: &MExpr, expr2: &MExpr) -> bool {
        match (expr1, expr2) {
            (
                MExpr::LetIn { decls, cont },
                MExpr::LetIn {
                    decls: decls_,
                    cont: cont_,
                },
            ) => {
                decls.len() == decls_.len()
                    && decls
                        .iter()
                        .zip(decls_.iter())
                        .all(|(decl, decl_)| self.eq_decl(decl, decl_))
                    && self.eq_expr(cont, cont_)
            }
            (
                MExpr::UnOp {
                    bind,
                    prim,
                    arg1,
                    cont,
                },
                MExpr::UnOp {
                    bind: bind_,
                    prim: prim_,
                    arg1: arg1_,
                    cont: cont_,
                },
            ) => {
                self.eq_ident(bind, bind_)
                    && prim == prim_
                    && self.eq_atom(arg1, arg1_)
                    && self.eq_expr(cont, cont_)
            }
            (
                MExpr::BinOp {
                    bind,
                    prim,
                    arg1,
                    arg2,
                    cont,
                },
                MExpr::BinOp {
                    bind: bind_,
                    prim: prim_,
                    arg1: arg1_,
                    arg2: arg2_,
                    cont: cont_,
                },
            ) => {
                self.eq_ident(bind, bind_)
                    && prim == prim_
                    && self.eq_atom(arg1, arg1_)
                    && self.eq_atom(arg2, arg2_)
                    && self.eq_expr(cont, cont_)
            }
            (
                MExpr::Call {
                    bind,
                    func,
                    args,
                    cont,
                },
                MExpr::Call {
                    bind: bind_,
                    func: func_,
                    args: args_,
                    cont: cont_,
                },
            ) => {
                self.eq_ident(bind, bind_)
                    && self.eq_atom(func, func_)
                    && args.len() == args_.len()
                    && args
                        .iter()
                        .zip(args_.iter())
                        .all(|(arg, arg_)| self.eq_atom(arg, arg_))
                    && self.eq_expr(cont, cont_)
            }
            (MExpr::Retn { arg1 }, MExpr::Retn { arg1: arg1_ }) => self.eq_atom(arg1, arg1_),
            (
                MExpr::Alloc { bind, size, cont },
                MExpr::Alloc {
                    bind: bind_,
                    size: size_,
                    cont: cont_,
                },
            ) => self.eq_ident(bind, bind_) && size == size_ && self.eq_expr(cont, cont_),
            (
                MExpr::Load {
                    bind,
                    arg1,
                    index,
                    cont,
                },
                MExpr::Load {
                    bind: bind_,
                    arg1: arg1_,
                    index: index_,
                    cont: cont_,
                },
            ) => {
                self.eq_ident(bind, bind_)
                    && self.eq_atom(arg1, arg1_)
                    && index == index_
                    && self.eq_expr(cont, cont_)
            }
            (
                MExpr::Store {
                    arg1,
                    index,
                    arg2,
                    cont,
                },
                MExpr::Store {
                    arg1: arg1_,
                    index: index_,
                    arg2: arg2_,
                    cont: cont_,
                },
            ) => {
                self.eq_atom(arg1, arg1_)
                    && index == index_
                    && self.eq_atom(arg2, arg2_)
                    && self.eq_expr(cont, cont_)
            }
            (
                MExpr::Offset {
                    bind,
                    arg1,
                    index,
                    cont,
                },
                MExpr::Offset {
                    bind: bind_,
                    arg1: arg1_,
                    index: index_,
                    cont: cont_,
                },
            ) => {
                self.eq_ident(bind, bind_)
                    && self.eq_atom(arg1, arg1_)
                    && index == index_
                    && self.eq_expr(cont, cont_)
            }
            (
                MExpr::Ifte {
                    bind,
                    arg1,
                    brch1,
                    brch2,
                    cont,
                },
                MExpr::Ifte {
                    bind: bind_,
                    arg1: arg1_,
                    brch1: brch1_,
                    brch2: brch2_,
                    cont: cont_,
                },
            ) => {
                self.eq_ident(bind, bind_)
                    && self.eq_atom(arg1, arg1_)
                    && self.eq_expr(brch1, brch1_)
                    && self.eq_expr(brch2, brch2_)
                    && self.eq_expr(cont, cont_)
            }
            (
                MExpr::Switch {
                    bind,
                    arg1,
                    brchs,
                    cont,
                },
                MExpr::Switch {
                    bind: bind_,
                    arg1: arg1_,
                    brchs: brchs_,
                    cont: cont_,
                },
            ) => {
                self.eq_ident(bind, bind_)
                    && self.eq_atom(arg1, arg1_)
                    && brchs.len() == brchs_.len()
                    && brchs
                        .iter()
                        .zip(brchs_.iter())
                        .all(|(brch, brch_)| self.eq_expr(brch, brch_))
                    && self.eq_expr(cont, cont_)
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
    pub fn unit() -> Atom {
        Atom::Unit
    }
    pub fn v(x: &str) -> Atom {
        Atom::Var(intern(x).as_dummy())
    }
    pub fn name(x: &str) -> Unique {
        intern(x).as_dummy()
    }
    pub fn fun(func: &str, pars: Vec<&str>, body: MExpr) -> MDecl {
        MDecl {
            func: name(func),
            pars: pars.into_iter().map(|par| name(par)).collect(),
            body,
        }
    }
    pub fn chain(vec: Vec<MExpr>) -> MExpr {
        vec.into_iter()
            .rev()
            .reduce(|e1, e2| match e2 {
                MExpr::LetIn { decls, cont } => {
                    assert!(cont.is_retn());
                    let cont = Box::new(e1);
                    MExpr::LetIn { decls, cont }
                }
                MExpr::UnOp {
                    bind,
                    prim,
                    arg1,
                    cont,
                } => {
                    assert!(cont.is_retn());
                    let cont = Box::new(e1);
                    MExpr::UnOp {
                        bind,
                        prim,
                        arg1,
                        cont,
                    }
                }
                MExpr::BinOp {
                    bind,
                    prim,
                    arg1,
                    arg2,
                    cont,
                } => {
                    assert!(cont.is_retn());
                    let cont = Box::new(e1);
                    MExpr::BinOp {
                        bind,
                        prim,
                        arg1,
                        arg2,
                        cont,
                    }
                }
                MExpr::Call {
                    bind,
                    func,
                    args,
                    cont,
                } => {
                    assert!(cont.is_retn());
                    let cont = Box::new(e1);
                    MExpr::Call {
                        bind,
                        func,
                        args,
                        cont,
                    }
                }
                MExpr::Retn { arg1 } => MExpr::Retn { arg1 },
                MExpr::Alloc { bind, size, cont } => {
                    assert!(cont.is_retn());
                    let cont = Box::new(e1);
                    MExpr::Alloc { bind, size, cont }
                }
                MExpr::Load {
                    bind,
                    arg1,
                    index,
                    cont,
                } => {
                    assert!(cont.is_retn());
                    let cont = Box::new(e1);
                    MExpr::Load {
                        bind,
                        arg1,
                        index,
                        cont,
                    }
                }
                MExpr::Store {
                    arg1,
                    index,
                    arg2,
                    cont,
                } => {
                    assert!(cont.is_retn());
                    let cont = Box::new(e1);
                    MExpr::Store {
                        arg1,
                        index,
                        arg2,
                        cont,
                    }
                }
                MExpr::Offset {
                    bind,
                    arg1,
                    index,
                    cont,
                } => {
                    assert!(cont.is_retn());
                    let cont = Box::new(e1);
                    MExpr::Offset {
                        bind,
                        arg1,
                        index,
                        cont,
                    }
                }
                MExpr::Ifte {
                    bind,
                    arg1,
                    brch1,
                    brch2,
                    cont,
                } => {
                    assert!(cont.is_retn());
                    let cont = Box::new(e1);
                    MExpr::Ifte {
                        bind,
                        arg1,
                        brch1,
                        brch2,
                        cont,
                    }
                }
                MExpr::Switch {
                    bind,
                    arg1,
                    brchs,
                    cont,
                } => {
                    assert!(cont.is_retn());
                    let cont = Box::new(e1);
                    MExpr::Switch {
                        bind,
                        arg1,
                        brchs,
                        cont,
                    }
                }
            })
            .unwrap()
    }
    pub fn let_in(decls: Vec<MDecl>, vec: Vec<MExpr>) -> MExpr {
        MExpr::LetIn {
            decls,
            cont: Box::new(chain(vec)),
        }
    }
    #[inline]
    pub fn binop(bind: &str, prim: BinOpPrim, arg1: Atom, arg2: Atom) -> MExpr {
        let bind = name(bind);
        let cont = Box::new(MExpr::Retn {
            arg1: Atom::Var(bind),
        });
        MExpr::BinOp {
            bind,
            prim,
            arg1,
            arg2,
            cont,
        }
    }
    pub fn iadd(bind: &str, arg1: Atom, arg2: Atom) -> MExpr {
        binop(bind, BinOpPrim::IAdd, arg1, arg2)
    }
    pub fn isub(bind: &str, arg1: Atom, arg2: Atom) -> MExpr {
        binop(bind, BinOpPrim::ISub, arg1, arg2)
    }
    pub fn imul(bind: &str, arg1: Atom, arg2: Atom) -> MExpr {
        binop(bind, BinOpPrim::IMul, arg1, arg2)
    }
    #[inline]
    pub fn unop(bind: &str, prim: UnOpPrim, arg1: Atom) -> MExpr {
        let bind = name(bind);
        let cont = Box::new(MExpr::Retn {
            arg1: Atom::Var(bind),
        });
        MExpr::UnOp {
            bind,
            prim,
            arg1,
            cont,
        }
    }
    // well, `move` is a rust keyword!
    pub fn _move(bind: &str, arg1: Atom) -> MExpr {
        unop(bind, UnOpPrim::Move, arg1)
    }
    pub fn alloc(bind: &str, size: usize) -> MExpr {
        let bind = name(bind);
        let cont = Box::new(MExpr::Retn {
            arg1: Atom::Var(bind),
        });
        MExpr::Alloc { bind, size, cont }
    }
    pub fn load(bind: &str, arg1: Atom, index: usize) -> MExpr {
        let bind = name(bind);
        let cont = Box::new(MExpr::Retn {
            arg1: Atom::Var(bind),
        });
        MExpr::Load {
            bind,
            arg1,
            index,
            cont,
        }
    }
    pub fn store(arg1: Atom, index: usize, arg2: Atom) -> MExpr {
        let bind = name("_");
        let cont = Box::new(MExpr::Retn {
            arg1: Atom::Var(bind),
        });
        MExpr::Store {
            arg1,
            index,
            arg2,
            cont,
        }
    }
    pub fn offset(bind: &str, arg1: Atom, index: usize) -> MExpr {
        let bind = name(bind);
        let cont = Box::new(MExpr::Retn {
            arg1: Atom::Var(bind),
        });
        MExpr::Offset {
            bind,
            arg1,
            index,
            cont,
        }
    }
    pub fn ifte(bind: &str, arg1: Atom, brch1: MExpr, brch2: MExpr) -> MExpr {
        let bind = name(bind);
        let brch1 = Box::new(brch1);
        let brch2 = Box::new(brch2);
        let cont = Box::new(MExpr::Retn {
            arg1: Atom::Var(bind),
        });
        MExpr::Ifte {
            bind,
            arg1,
            brch1,
            brch2,
            cont,
        }
    }
    pub fn switch(bind: &str, arg1: Atom, brchs: Vec<MExpr>) -> MExpr {
        let bind = name(bind);
        let cont = Box::new(MExpr::Retn {
            arg1: Atom::Var(bind),
        });
        MExpr::Switch {
            bind,
            arg1,
            brchs,
            cont,
        }
    }
    pub fn call(bind: &str, func: &str, args: Vec<Atom>) -> MExpr {
        let bind = name(bind);
        let func = Atom::Var(name(func));
        let cont = Box::new(MExpr::Retn {
            arg1: Atom::Var(bind),
        });
        MExpr::Call {
            bind,
            func,
            args,
            cont,
        }
    }
    /*
    pub fn call_ext(bind: &str, func: &str, args: Vec<Atom>) -> MExpr {
        todo!()
    }
    */
    pub fn retn(arg1: Atom) -> MExpr {
        MExpr::Retn { arg1 }
    }
}

#[test]
#[ignore]
fn anf_build_test() {
    use self::anf_build::*;
    let expr = chain(vec![
        iadd("a", i(42), i(1)),
        iadd("b", v("a"), v("a")),
        ifte(
            "c",
            v("a"),
            chain(vec![iadd("c", v("a"), v("a"))]),
            retn(v("a")),
        ),
        retn(v("c")),
    ]);
    println!("{expr}")
}

#[test]
fn alpha_equiv_test() {
    use self::anf_build::*;
    let expr1 = chain(vec![
        iadd("x", i(1), i(2)),
        imul("y", v("x"), v("x")),
        retn(v("y")),
    ]);

    let expr2 = chain(vec![
        iadd("a", i(1), i(2)),
        imul("b", v("a"), v("a")),
        retn(v("b")),
    ]);

    let expr3 = chain(vec![
        iadd("a", i(1), i(2)),
        imul("b", v("a"), v("a")),
        retn(v("a")),
    ]);

    assert_eq!(expr1, expr2);
    assert_ne!(expr1, expr3);
    assert_ne!(expr2, expr3);
}
