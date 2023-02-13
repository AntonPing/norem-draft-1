use super::*;
use crate::utils::env_map::EnvMap;

pub fn run_const_fold(expr: MExpr) -> MExpr {
    let mut pass = ConstFold::new();
    pass.visit_expr(expr)
}

#[derive(Clone, Debug)]
pub struct ConstFold {
    atom_map: EnvMap<Ident, Atom>,
    alloc_map: EnvMap<Ident, usize>,
    store_map: EnvMap<(Ident, usize), Atom>,
    offset_map: EnvMap<Ident, (Ident, usize)>,
    ret_stack: Vec<(Ident, MExpr)>,
}

impl ConstFold {
    fn new() -> ConstFold {
        ConstFold {
            atom_map: EnvMap::new(),
            alloc_map: EnvMap::new(),
            store_map: EnvMap::new(),
            offset_map: EnvMap::new(),
            ret_stack: Vec::new(),
        }
    }
    #[allow(dead_code)]
    fn get_real_addr(&self, var: Ident, index: usize) -> (Ident, usize) {
        self.offset_map
            .get(&var)
            .map(|(var2, index2)| {
                assert!(self.alloc_map.contains_key(&var2));
                assert!(self.alloc_map[&var2] > *index2 + index);
                (*var2, *index2 + index)
            })
            .unwrap_or_else(|| {
                assert!(self.alloc_map.contains_key(&var));
                assert!(self.alloc_map[&var] > index);
                (var, index)
            })
    }
    fn enter_scope(&mut self) {
        self.atom_map.enter_scope();
        self.alloc_map.enter_scope();
        self.store_map.enter_scope();
        self.offset_map.enter_scope();
    }
    fn leave_scope(&mut self) {
        self.atom_map.leave_scope();
        self.alloc_map.leave_scope();
        self.store_map.leave_scope();
        self.offset_map.leave_scope();
    }
    fn visit_brch(&mut self, brch: MExpr) -> MExpr {
        self.enter_scope();
        let res = self.visit_expr(brch);
        self.leave_scope();
        res
    }
    fn visit_decl(&mut self, decl: MDecl) -> MDecl {
        let MDecl { func, pars, body } = decl;
        self.enter_scope();
        let body = self.visit_expr(body);
        self.leave_scope();
        MDecl { func, pars, body }
    }
    fn visit_arg(&mut self, arg: Atom) -> Atom {
        // substitute until it's constant or no binding
        let mut atom = arg;
        loop {
            if let Atom::Var(sym) = atom {
                if let Some(res) = self.atom_map.get(&sym) {
                    atom = *res
                } else {
                    return atom;
                }
            } else {
                return atom;
            }
        }
    }
    fn visit_expr(&mut self, expr: MExpr) -> MExpr {
        let expr = expr.walk_arg(|arg| self.visit_arg(arg));
        let expr = match expr {
            MExpr::UnOp {
                bind,
                prim,
                arg1,
                cont,
            } => {
                use Atom::*;
                use UnOpPrim::*;
                match &(prim, arg1) {
                    (Move, arg1) => {
                        self.atom_map.insert(bind, *arg1);
                        return self.visit_expr(*cont);
                    }
                    (INeg, Int(a)) => {
                        self.atom_map.insert(bind, Int(-a));
                        return self.visit_expr(*cont);
                    }
                    _ => {}
                }
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
                use Atom::*;
                use BinOpPrim::*;
                match &(prim, arg1, arg2) {
                    // a + b
                    (IAdd, Int(a), Int(b)) => {
                        self.atom_map.insert(bind, Int(a + b));
                        return self.visit_expr(*cont);
                    }
                    // x + 0 = 0 + x = x
                    (IAdd, Var(x), Int(0)) | (IAdd, Int(0), Var(x)) => {
                        self.atom_map.insert(bind, Var(*x));
                        return self.visit_expr(*cont);
                    }
                    // a - b
                    (ISub, Int(a), Int(b)) => {
                        self.atom_map.insert(bind, Int(a - b));
                        return self.visit_expr(*cont);
                    }
                    // x - 0 = x
                    (ISub, Var(x), Int(0)) => {
                        self.atom_map.insert(bind, Var(*x));
                        return self.visit_expr(*cont);
                    }
                    // x - x = 0
                    (ISub, Var(x), Var(y)) if x == y => {
                        self.atom_map.insert(bind, Int(0));
                        return self.visit_expr(*cont);
                    }
                    // a * b
                    (IMul, Int(a), Int(b)) => {
                        self.atom_map.insert(bind, Int(a * b));
                        return self.visit_expr(*cont);
                    }
                    // x * 0 = 0 * x = 0
                    (IMul, Var(_x), Int(0)) | (IMul, Int(0), Var(_x)) => {
                        self.atom_map.insert(bind, Int(0));
                        return self.visit_expr(*cont);
                    }
                    // x * 1 = 1 * x = x
                    (IMul, Var(x), Int(1)) | (IMul, Int(1), Var(x)) => {
                        self.atom_map.insert(bind, Var(*x));
                        return self.visit_expr(*cont);
                    }
                    _ => {}
                }
                MExpr::BinOp {
                    bind,
                    prim,
                    arg1,
                    arg2,
                    cont,
                }
            }
            MExpr::Alloc { bind, size, cont } => {
                // self.alloc_map.insert(bind, size);
                MExpr::Alloc { bind, size, cont }
            }
            MExpr::Load {
                bind,
                arg1,
                index,
                cont,
            } => {
                /*
                let var = arg1.unwrap_var();
                let addr = self.get_real_addr(var, index);
                if let Some(res) = self.store_map.get(&addr) {
                    self.atom_map.insert(bind, *res);
                    return self.visit_expr(*cont);
                }
                */
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
                /*
                let var = arg1.unwrap_var();
                let addr = self.get_real_addr(var, index);
                self.store_map.insert(addr, arg2);
                */
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
                if index == 0 {
                    self.atom_map.insert(bind, arg1);
                    return self.visit_expr(*cont);
                } else {
                    /*
                    let var = arg1.unwrap_var();
                    let addr = self.get_real_addr(var, index);
                    self.offset_map.insert(bind, addr);
                    */
                    MExpr::Offset {
                        bind,
                        arg1,
                        index,
                        cont,
                    }
                }
            }
            MExpr::Ifte {
                bind,
                arg1,
                brch1,
                brch2,
                cont,
            } => {
                if let Atom::Bool(p) = arg1 {
                    self.ret_stack.push((bind, *cont));
                    if p {
                        return self.visit_expr(*brch1);
                    } else {
                        return self.visit_expr(*brch2);
                    }
                } else {
                    MExpr::Ifte {
                        bind,
                        arg1,
                        brch1,
                        brch2,
                        cont,
                    }
                }
            }
            MExpr::Switch {
                bind,
                arg1,
                brchs,
                dflt,
                cont,
            } => {
                if let Atom::Int(x) = arg1 {
                    self.ret_stack.push((bind, *cont));
                    for (i, brch) in brchs.into_iter() {
                        if i == x as usize {
                            return self.visit_expr(brch);
                        }
                    }
                    if let Some(dflt) = dflt {
                        return self.visit_expr(*dflt);
                    }
                    panic!("pattern match not exhaustive!");
                } else {
                    MExpr::Switch {
                        bind,
                        arg1,
                        brchs,
                        dflt,
                        cont,
                    }
                }
            }
            MExpr::Retn { arg1 } => {
                if let Some((bind, cont)) = self.ret_stack.pop() {
                    self.atom_map.insert(bind, arg1);
                    return self.visit_expr(cont);
                } else {
                    MExpr::Retn { arg1 }
                }
            }
            other => other,
        };
        expr.walk_brch(|brch| self.visit_brch(brch))
            .walk_decl(|decl| self.visit_decl(decl))
            .walk_cont(|cont| self.visit_expr(cont))
    }
}

#[test]
fn const_fold_test_arithmetic() {
    // test arithmetic operation optimization(and move operation)
    use super::anf_build::*;
    let expr1 = chain(vec![
        iadd("x", i(1), i(2)),
        _move("y", v("x")),
        isub("z", v("y"), i(1)),
        imul("r", v("z"), i(3)),
        retn(v("r")),
    ]);
    let expr1 = run_const_fold(expr1);
    let expr2 = retn(i(6));
    assert_eq!(expr1, expr2);
}

#[test]
fn const_fold_test_ifte() {
    // test if-then-else folding
    use super::anf_build::*;
    let expr1 = chain(vec![
        _move("x", i(42)),
        ifte(
            "j",
            b(false),
            chain(vec![iadd("y", v("x"), i(1)), retn(v("y"))]),
            chain(vec![iadd("z", v("x"), i(2)), retn(v("z"))]),
        ),
        retn(v("j")),
    ]);
    let expr1 = run_const_fold(expr1);
    let expr2 = retn(i(44));
    assert_eq!(expr1, expr2);
}

#[test]
fn const_fold_test_switch() {
    // test switch folding
    use super::anf_build::*;
    let expr1 = chain(vec![
        _move("x", i(42)),
        switch(
            "j",
            i(1),
            vec![
                (1, chain(vec![iadd("y1", v("x"), i(1)), retn(v("y1"))])),
                (2, chain(vec![iadd("y2", v("x"), i(2)), retn(v("y2"))])),
                (3, chain(vec![iadd("y3", v("x"), i(3)), retn(v("y3"))])),
            ],
            None,
        ),
        retn(v("j")),
    ]);
    let expr1 = run_const_fold(expr1);
    let expr2 = retn(i(43));
    assert_eq!(expr1, expr2);
}
#[test]
#[ignore = "commented, implementation wrong"]
fn const_fold_test_store_load() {
    // test alloc, store and offset optimization
    use super::anf_build::*;
    let expr1 = chain(vec![
        alloc("m", 3),
        store(v("m"), 0, i(1)),
        store(v("m"), 1, i(2)),
        store(v("m"), 2, i(3)),
        offset("n", v("m"), 1),
        load("x", v("n"), 0),
        load("y", v("n"), 1),
        iadd("z", v("x"), v("y")),
        retn(v("z")),
    ]);
    let expr1 = run_const_fold(expr1);
    let expr2 = chain(vec![
        alloc("m", 3),
        store(v("m"), 0, i(1)),
        store(v("m"), 1, i(2)),
        store(v("m"), 2, i(3)),
        offset("n", v("m"), 1),
        retn(i(5)),
    ]);
    assert_eq!(expr1, expr2);
}

/*
    // some critical case
    let expr1 = chain(vec![
        alloc("m", 1),
        ifte(
            "_",
            v("?"),
            chain(vec![
                load("r1", v("m"), 0),
                store(v("m"), 0, i(1)),
                retn(v("r1")),
            ]),
            chain(vec![
                load("r2", v("m"), 0),
                store(v("m"), 0, i(2)),
                retn(v("r2")),
            ]),
        ),
        load("r", v("m"), 0),
        store(v("m"), 0, i(3)),
        retn(v("r")),
    ]);
    let expr1 = ConstFold::run(expr1);
    println!("{expr1}");
*/
