use super::*;
use crate::utils::env_map::{EnvMap, FreeSet};
use std::collections::{HashMap, HashSet};

pub fn run_dead_elim(expr: MExpr) -> MExpr {
    let mut pass = DeadElim::new();
    pass.visit_expr(expr)
}

fn fix_point<T>(used: HashSet<T>, graph: HashMap<T, HashSet<T>>) -> HashSet<T>
where
    T: std::hash::Hash + Copy + Eq,
{
    let mut set = used;
    let mut new = HashSet::new();
    loop {
        for key in &set {
            if let Some(val) = graph.get(&key) {
                for x in val {
                    if !set.contains(&x) {
                        new.insert(*x);
                    }
                }
            }
        }
        if new.is_empty() {
            return set;
        } else {
            set.extend(new.drain())
        }
    }
}

pub struct DeadElim {
    free_set: FreeSet<Ident>,
    load_map: EnvMap<Ident, HashSet<usize>>,
    ret_used: Vec<bool>,
}

impl DeadElim {
    fn new() -> DeadElim {
        DeadElim {
            free_set: FreeSet::new(),
            load_map: EnvMap::new(),
            ret_used: vec![true],
        }
    }
    fn enter_scope(&mut self) {
        self.free_set.enter_scope();
        self.load_map.enter_scope();
    }
    fn leave_scope(&mut self) {
        self.free_set.leave_scope();
        self.load_map.leave_scope();
    }
    fn visit_arg(&mut self, arg: Atom) -> Atom {
        if let Atom::Var(var) = arg {
            self.free_set.insert(var);
        }
        arg
    }
    fn visit_expr(&mut self, expr: MExpr) -> MExpr {
        let expr = expr.walk_cont(|cont| self.visit_expr(cont));
        let expr = match expr {
            MExpr::LetIn { decls, cont } => {
                let used: HashSet<Ident> = decls
                    .iter()
                    .map(|decl| decl.func)
                    .filter(|func| self.free_set.contains(func))
                    .collect();

                let names: HashSet<Ident> = decls.iter().map(|decl| decl.func).collect();

                let (decls, sets): (Vec<MDecl>, Vec<HashSet<Ident>>) = decls
                    .into_iter()
                    .map(|decl| {
                        let MDecl { func, pars, body } = decl;
                        self.enter_scope();
                        let body = self.visit_expr(body);
                        let set = self
                            .free_set
                            .iter()
                            .filter(|var| names.contains(var))
                            .cloned()
                            .collect();
                        self.leave_scope();
                        (MDecl { func, pars, body }, set)
                    })
                    .unzip();

                let graph: HashMap<Ident, HashSet<Ident>> = decls
                    .iter()
                    .map(|decl| decl.func)
                    .zip(sets.into_iter())
                    .collect();

                let reachable = fix_point(used, graph);
                let decls: Vec<MDecl> = decls
                    .into_iter()
                    .filter(|decl| reachable.contains(&decl.func))
                    .collect();

                if decls.is_empty() {
                    return *cont;
                } else {
                    MExpr::LetIn { decls, cont }
                }
            }
            MExpr::UnOp {
                bind,
                prim,
                arg1,
                cont,
            } => {
                if !self.free_set.contains(&bind) {
                    return *cont;
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
                if !self.free_set.contains(&bind) {
                    return *cont;
                }
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
                if self.free_set.contains(&bind) {
                    // todo: pure dead-call elimination
                }
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
            } => MExpr::ExtCall {
                bind,
                func,
                args,
                cont,
            },
            MExpr::Retn { arg1 } => {
                if *self.ret_used.last().unwrap() {
                    MExpr::Retn { arg1 }
                } else {
                    MExpr::Retn { arg1: Atom::Unit }
                }
            }
            MExpr::Alloc { bind, size, cont } => {
                if !self.free_set.contains(&bind) {
                    return *cont;
                }
                MExpr::Alloc { bind, size, cont }
            }
            MExpr::Load {
                bind,
                arg1,
                index,
                cont,
            } => {
                if !self.free_set.contains(&bind) {
                    return *cont;
                }
                /*
                let var = arg1.unwrap_variable();
                // todo: better api (like `get_mut`) to avoid clone
                let mut set = self.load_map.get(&var).unwrap().clone();
                set.insert(index);
                self.load_map.insert(var, set);
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
                let var = arg1.unwrap_variable();
                let set = self.load_map.get(&var).unwrap();
                if set.is_empty() {
                    return *cont;
                } else {
                    MExpr::Store { arg1, index, arg2, cont }
                }
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
                if !self.free_set.contains(&bind) {
                    return *cont;
                }
                /*
                let var = arg1.unwrap_variable();
                let set = self.load_map.get(&var)
                    .unwrap().iter()
                    .flat_map(|n| if *n >= index { Some(n - index) } else {None})
                    .collect();
                self.load_map.insert(var, set);
                */
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
                if !self.free_set.contains(&bind) {
                    self.ret_used.push(false)
                } else {
                    self.ret_used.push(true)
                }
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
                if !self.free_set.contains(&bind) {
                    self.ret_used.push(false)
                } else {
                    self.ret_used.push(true)
                }
                MExpr::Switch {
                    bind,
                    arg1,
                    brchs,
                    dflt,
                    cont,
                }
            }
        };

        let expr = expr.walk_brch(|brch| {
            self.enter_scope();
            let res = self.visit_expr(brch);
            self.leave_scope();
            res
        });

        match &expr {
            MExpr::Ifte { .. } => {
                self.ret_used.pop();
            }
            MExpr::Switch { .. } => {
                self.ret_used.pop();
            }
            _ => {}
        }

        expr.walk_arg(|arg| self.visit_arg(arg))
    }
}

#[test]
fn dead_elim_test_arithmetic() {
    // test dead arithmetic operation elimination
    use super::anf_build::*;
    let expr1 = chain(vec![
        iadd("x", i(1), i(1)),
        iadd("y", v("x"), i(1)),
        iadd("z", v("y"), i(1)),
        retn(v("x")),
    ]);
    let expr1 = run_dead_elim(expr1);
    let expr2 = chain(vec![iadd("x", i(1), i(1)), retn(v("x"))]);
    assert_eq!(expr1, expr2);
}

#[test]
fn dead_elim_test_branch() {
    // test unused branch result
    use super::anf_build::*;
    let expr1 = chain(vec![
        iadd("x", i(1), i(1)),
        ifte(
            "_",
            v("?"),
            chain(vec![iadd("y", v("x"), i(1)), retn(v("y"))]),
            chain(vec![retn(v("x"))]),
        ),
        retn(v("x")),
    ]);
    let expr1 = run_dead_elim(expr1);
    let expr2 = chain(vec![
        iadd("x", i(1), i(1)),
        ifte(
            "_",
            v("?"),
            chain(vec![retn(unit())]),
            chain(vec![retn(unit())]),
        ),
        retn(v("x")),
    ]);
    assert_eq!(expr1, expr2);
}

#[test]
fn dead_elim_test_function() {
    // test unused function declaration
    use super::anf_build::*;
    let expr1 = let_in(
        vec![
            fun(
                "f1",
                vec!["x1"],
                chain(vec![call("r1", "f2", vec![v("x1")]), retn(v("r1"))]),
            ),
            fun(
                "f2",
                vec!["x2"],
                chain(vec![call("r2", "f1", vec![v("x2")]), retn(v("r2"))]),
            ),
            fun(
                "f3",
                vec!["x3"],
                chain(vec![call("r3", "f3", vec![v("x3")]), retn(v("r3"))]),
            ),
            fun("f4", vec!["x4"], chain(vec![retn(v("x4"))])),
        ],
        vec![call("y1", "f1", vec![i(1)]), retn(v("y1"))],
    );
    let expr1 = run_dead_elim(expr1);
    let expr2 = let_in(
        vec![
            fun(
                "f1",
                vec!["x1"],
                chain(vec![call("r1", "f2", vec![v("x1")]), retn(v("r1"))]),
            ),
            fun(
                "f2",
                vec!["x2"],
                chain(vec![call("r2", "f1", vec![v("x2")]), retn(v("r2"))]),
            ),
        ],
        vec![call("y1", "f1", vec![i(1)]), retn(v("y1"))],
    );
    assert_eq!(expr1, expr2);
}
