use super::*;

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
    Atom::Var(Ident::from(InternStr::new(x)))
}
pub fn name(x: &str) -> Ident {
    Ident::from(InternStr::new(x))
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
            MExpr::ExtCall {
                bind,
                func,
                args,
                cont,
            } => {
                assert!(cont.is_retn());
                let cont = Box::new(e1);
                MExpr::ExtCall {
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
                dflt,
                cont,
            } => {
                assert!(cont.is_retn());
                let cont = Box::new(e1);
                MExpr::Switch {
                    bind,
                    arg1,
                    brchs,
                    dflt,
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
pub fn offset(bind: &str, arg1: Atom, index: isize) -> MExpr {
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
pub fn switch(bind: &str, arg1: Atom, brchs: Vec<(usize, MExpr)>, dflt: Option<MExpr>) -> MExpr {
    let bind = name(bind);
    let cont = Box::new(MExpr::Retn {
        arg1: Atom::Var(bind),
    });
    let dflt = dflt.map(|x| Box::new(x));
    MExpr::Switch {
        bind,
        arg1,
        brchs,
        dflt,
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
