use super::*;
use std::collections::HashMap;

struct AlphaEquiv {
    map: HashMap<Ident, Ident>,
}

impl AlphaEquiv {
    fn new() -> AlphaEquiv {
        AlphaEquiv {
            map: HashMap::new(),
        }
    }

    fn eq_ident(&mut self, ident1: &Ident, ident2: &Ident) -> bool {
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
                    dflt,
                    cont,
                },
                MExpr::Switch {
                    bind: bind_,
                    arg1: arg1_,
                    brchs: brchs_,
                    dflt: dflt_,
                    cont: cont_,
                },
            ) => {
                self.eq_ident(bind, bind_)
                    && self.eq_atom(arg1, arg1_)
                    && brchs.len() == brchs_.len()
                    && brchs
                        .iter()
                        .zip(brchs_.iter())
                        .all(|((i, brch), (i_, brch_))| i == i_ && self.eq_expr(brch, brch_))
                    && match (dflt, dflt_) {
                        (Some(dflt), Some(dflt_)) => self.eq_expr(dflt, dflt_),
                        (None, None) => true,
                        _ => false,
                    }
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
