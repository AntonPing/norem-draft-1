use super::*;
use std::collections::{HashMap, HashSet};

pub fn run_linear_inline(expr: MExpr) -> MExpr {
    let (expr, set) = LinearInlineScan::run(expr);
    InlinePerform::run(expr, set)
}

struct LinearInlineScan {
    // map a unique identifier to (n,m)
    // where n is occur times, and m is call-site occur times
    // n >= m always
    occur: HashMap<Ident, (usize, usize)>,
    set: HashSet<Ident>,
}

impl LinearInlineScan {
    fn new() -> LinearInlineScan {
        LinearInlineScan {
            occur: HashMap::new(),
            set: HashSet::new(),
        }
    }
    fn run(expr: MExpr) -> (MExpr, HashSet<Ident>) {
        let mut pass = LinearInlineScan::new();
        let res = pass.visit_expr(expr);
        (res, pass.set)
    }

    fn visit_expr(&mut self, expr: MExpr) -> MExpr {
        let expr = expr
            .walk_brch(|brch| self.visit_expr(brch))
            .walk_decl(|decl| self.visit_decl(decl))
            .walk_cont(|cont| self.visit_expr(cont))
            .walk_arg(|arg| self.visit_atom(arg));

        let expr = match expr {
            MExpr::LetIn { decls, cont } => {
                for decl in decls.iter() {
                    let (n, m) = self.occur.get(&decl.func).unwrap_or(&(0, 0)).clone();
                    assert!(n >= m);
                    if n == 1 && m == 1 {
                        self.set.insert(decl.func);
                    }
                }
                MExpr::LetIn { decls, cont }
            }
            MExpr::Call {
                bind,
                func,
                args,
                cont,
            } => {
                let var = func.unwrap_var();
                let (_n, m) = self.occur.get_mut(&var).unwrap();
                *m += 1;
                MExpr::Call {
                    bind,
                    func,
                    args,
                    cont,
                }
            }
            other => other,
        };
        expr
    }

    fn visit_atom(&mut self, atom: Atom) -> Atom {
        if let Atom::Var(var) = atom {
            if let Some((n, _m)) = self.occur.get_mut(&var) {
                *n += 1;
            } else {
                self.occur.insert(var, (1, 0));
            }
        }
        atom
    }

    fn visit_decl(&mut self, decl: MDecl) -> MDecl {
        let MDecl { func, pars, body } = decl;
        let body = self.visit_expr(body);
        MDecl { func, pars, body }
    }
}

fn concat(expr: MExpr, bind: Ident, cont: MExpr) -> MExpr {
    match expr {
        MExpr::Retn { arg1 } => MExpr::UnOp {
            bind,
            prim: UnOpPrim::Move,
            arg1,
            cont: Box::new(cont),
        },
        other => other.walk_cont(|expr| concat(expr, bind, cont)),
    }
}

fn inline_call(decl: MDecl, bind: Ident, func: Ident, args: Vec<Atom>, cont: Box<MExpr>) -> MExpr {
    let MDecl {
        func: func2,
        pars,
        body,
    } = decl;
    assert_eq!(func, func2);
    assert_eq!(args.len(), pars.len());
    let new_expr = pars
        .into_iter()
        .zip(args.into_iter())
        .fold(body, |cont, (x, a)| MExpr::UnOp {
            bind: x,
            prim: UnOpPrim::Move,
            arg1: a,
            cont: Box::new(cont),
        });
    concat(new_expr, bind, *cont)
}

struct InlinePerform {
    linear_set: HashSet<Ident>,
    inline_map: HashMap<Ident, MDecl>,
}

impl InlinePerform {
    fn new(linear_set: HashSet<Ident>) -> InlinePerform {
        InlinePerform {
            linear_set,
            inline_map: HashMap::new(),
        }
    }
    fn run(expr: MExpr, linear_set: HashSet<Ident>) -> MExpr {
        let mut pass = InlinePerform::new(linear_set);
        assert!(pass.inline_map.is_empty());
        pass.visit_expr(expr)
    }

    fn visit_expr(&mut self, expr: MExpr) -> MExpr {
        let expr = match expr {
            MExpr::LetIn { decls, cont } => {
                let decls: Vec<MDecl> = decls
                    .into_iter()
                    .filter_map(|decl| {
                        if self.linear_set.contains(&decl.func) {
                            self.inline_map.insert(decl.func, decl);
                            None
                        } else {
                            Some(decl)
                        }
                    })
                    .collect();
                if decls.is_empty() {
                    return self.visit_expr(*cont);
                } else {
                    MExpr::LetIn { decls, cont }
                }
            }
            MExpr::Call {
                bind,
                func,
                args,
                cont,
            } => {
                let func = func.unwrap_var();
                if self.linear_set.contains(&func) {
                    let decl = self.inline_map.remove(&func).unwrap();
                    let decl = self.visit_decl(decl);
                    inline_call(decl, bind, func, args, cont)
                } else {
                    MExpr::Call {
                        bind,
                        func: Atom::Var(func),
                        args,
                        cont,
                    }
                }
            }
            other => other,
        };
        let expr = expr
            .walk_brch(|brch| self.visit_expr(brch))
            .walk_decl(|decl| self.visit_decl(decl))
            .walk_cont(|cont| self.visit_expr(cont));

        expr
    }

    fn visit_decl(&mut self, decl: MDecl) -> MDecl {
        let MDecl { func, pars, body } = decl;
        let body = self.visit_expr(body);
        MDecl { func, pars, body }
    }
}

#[test]
fn linear_inline_test_simple() {
    // test simple case of linear inline
    use super::anf_build::*;
    let expr1 = let_in(
        vec![fun(
            "f1",
            vec!["x1"],
            chain(vec![iadd("r1", v("x1"), i(1)), retn(v("r1"))]),
        )],
        vec![call("r2", "f1", vec![i(42)]), retn(v("r2"))],
    );
    let expr1 = run_linear_inline(expr1);
    let expr2 = chain(vec![
        _move("x1", i(42)),
        iadd("r1", v("x1"), i(1)),
        _move("r2", v("r1")),
        retn(v("r2")),
    ]);
    assert_eq!(expr1, expr2);
}

#[test]
fn linear_inline_test_chain() {
    // test chain effect of linear inline
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
                chain(vec![iadd("r2", v("x2"), i(1)), retn(v("r2"))]),
            ),
        ],
        vec![call("r3", "f1", vec![i(42)]), retn(v("r3"))],
    );
    let expr1 = run_linear_inline(expr1);
    let expr2 = chain(vec![
        _move("x1", i(42)),
        _move("x2", v("x1")),
        iadd("r1", v("x2"), i(1)),
        _move("r2", v("r1")),
        _move("r3", v("r2")),
        retn(v("r3")),
    ]);
    assert_eq!(expr1, expr2);
}

#[test]
fn linear_inline_test_complex() {
    // test complex case of linear inline
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
                chain(vec![iadd("r2", v("x2"), i(1)), retn(v("r2"))]),
            ),
        ],
        vec![
            call("t1", "f1", vec![i(42)]),
            call("t2", "f2", vec![i(42)]),
            iadd("r3", v("t1"), v("t2")),
            retn(v("r3")),
        ],
    );
    let expr1 = run_linear_inline(expr1);
    let expr2 = let_in(
        vec![fun(
            "f2",
            vec!["x2"],
            chain(vec![iadd("r2", v("x2"), i(1)), retn(v("r2"))]),
        )],
        vec![
            _move("t1", i(42)),
            call("t2", "f2", vec![v("t1")]),
            _move("t3", v("t2")),
            call("t4", "f2", vec![i(42)]),
            iadd("t5", v("t3"), v("t4")),
            retn(v("t5")),
        ],
    );
    assert_eq!(expr1, expr2);
}
