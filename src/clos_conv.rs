use std::collections::HashSet;

use crate::anf::*;
use crate::env_map::FreeSet;
use crate::intern::Unique;

pub struct ClosConv {
    toplevel: Vec<MDecl>,
    lifted: HashSet<Unique>,
    freevar: FreeSet<Unique>,
}

impl ClosConv {
    pub fn new() -> ClosConv {
        ClosConv {
            toplevel: Vec::new(),
            lifted: HashSet::new(),
            freevar: FreeSet::new(),
        }
    }
    pub fn run(expr: MExpr) -> MExpr {
        let mut pass = ClosConv::new();
        let expr = pass.visit_expr(expr);
        MExpr::LetIn {
            decls: pass.toplevel,
            // do renaming to obey the single-assignment rule
            cont: Box::new(expr),
        }
        .rename()
    }

    fn visit_bind(&mut self, bind: Unique) -> Unique {
        self.freevar.remove(&bind);
        bind
    }

    fn visit_arg(&mut self, atom: Atom) -> Atom {
        if let Atom::Var(sym) = atom {
            if !self.lifted.contains(&sym) {
                self.freevar.insert(sym);
            }
        }
        atom
    }

    fn visit_decl(&mut self, decl: MDecl) -> MDecl {
        let MDecl { func, pars, body } = decl;
        self.freevar.enter_scope();
        let body = self.visit_expr(body);
        self.freevar.leave_scope();
        self.freevar.remove(&func);
        for par in pars.iter() {
            self.freevar.remove(par);
        }
        MDecl { func, pars, body }
    }

    fn visit_expr(&mut self, expr: MExpr) -> MExpr {
        match expr {
            MExpr::LetIn { decls, cont } => {
                /*
                    letrec
                        foo(x,y,...,z) =
                            bar;
                    in
                        baz
                    end
                    =======> becomes =======>
                    letrec
                        foo(c,x,y,...,z) =
                            let v1 = load(c,1);
                            ...
                            let vn = load(c,n);
                            bar;
                    in
                        let c = alloc(n)
                        store(c,0,foo)
                        store(c,1,v1)
                        store(c,2,v2)
                        ...
                        store(c,n,vn)
                        baz[foo:=foo3]
                    end
                    where foo1, foo2, foo3 and c are generated variables
                */

                // record the order of function definition
                let func_names: Vec<Unique> = decls.iter().map(|decl| decl.func).collect();
                let c = Unique::generate('c');

                self.freevar.enter_scope();

                // transform function and get their free variable set
                let decls: Vec<_> = decls
                    .into_iter()
                    .map(|decl| self.visit_decl(decl))
                    .collect();

                for func in &func_names {
                    self.freevar.remove(func);
                    self.lifted.insert(*func);
                }

                // collect as vector to maintain the order
                let freevars: Vec<Unique> = self.freevar.iter().cloned().collect();

                self.freevar.leave_scope();

                // instead of returning the let-block: CExpr::Let { decls, cont }
                // we lift all decls to toplevel
                for (idx, decl) in decls.into_iter().enumerate() {
                    let MDecl {
                        func,
                        mut pars,
                        body,
                    } = decl;
                    pars.insert(0, c);
                    /*
                    foo(x,y,...,z) =
                        body_foo;
                    bar(x,y,...,z) =
                        body_bar;
                    =======> becomes =======>
                    foo(c,x,y,...,z) =
                        let foo = offset(c,0);
                        let bar = offset(c,1);
                        let v1 = load(c,2);
                        ...
                        let vn = load(c,n+1);
                        bar
                    bar(c,x,y,...,z) =
                        let foo = offset(c,-1);
                        let bar = offset(c,0);
                        let v1 = load(c,1);
                        ...
                        let vn = load(c,n);
                        bar
                    */
                    let body = freevars
                        .iter()
                        .enumerate()
                        .fold(body, |cont, (i, x)| MExpr::Load {
                            bind: *x,
                            arg1: Atom::Var(c),
                            index: i + func_names.len() - idx,
                            cont: Box::new(cont),
                        });
                    let body =
                        func_names
                            .iter()
                            .enumerate()
                            .fold(body, |cont, (i, x)| MExpr::Offset {
                                bind: *x,
                                arg1: Atom::Var(c),
                                index: (i as isize - idx as isize),
                                cont: Box::new(cont),
                            });
                    let decl = MDecl { func, pars, body };
                    self.toplevel.push(decl);
                }
                /*
                    (record creation)
                    let c = alloc(n+m)
                    (block functions)
                    store(c,0,f1')
                    store(c,1,f2')
                    ......
                    store(c,n-1,fn')
                    (free variables)
                    store(c,n,v1)
                    store(c,n+1,v1)
                    ...
                    store(c,n+m,v1)
                    (function offset)
                    let f1 = offset(c,0);
                    let f2 = offset(c,1);
                    ...
                    let fn = offset(c,n-1);
                    cont
                */

                let cont = Box::new(self.visit_expr(*cont));

                // generate shared closure 'c'
                let c = Unique::generate('c');

                // here is the (function offset) part
                let cont =
                    func_names
                        .iter()
                        .enumerate()
                        .fold(*cont, |expr, (i, f)| MExpr::Offset {
                            bind: *f,
                            arg1: Atom::Var(c),
                            index: i as isize,
                            cont: Box::new(expr),
                        });

                // here is the (block functions) and (free variables) part
                let cont = func_names.iter().chain(freevars.iter()).enumerate().fold(
                    cont,
                    |expr, (i, x)| MExpr::Store {
                        arg1: Atom::Var(c),
                        index: i,
                        arg2: Atom::Var(*x),
                        cont: Box::new(expr),
                    },
                );

                // here is the (record creation) part
                let cont = MExpr::Alloc {
                    bind: c,
                    size: func_names.len() + freevars.len(),
                    cont: Box::new(cont),
                };

                cont
            }
            MExpr::Call {
                bind,
                func,
                args,
                cont,
            } => {
                let cont = Box::new(self.visit_expr(*cont));
                let bind = self.visit_bind(bind);
                let func = self.visit_arg(func);
                let args: Vec<_> = args.into_iter().map(|arg| self.visit_arg(arg)).collect();
                /*
                    let bind = func(a,b,...,z);
                    cont;
                    =====> becomes =====>
                    let f = load(func,0);
                    let bind = f(func,a,b,...,z);
                    cont;
                */
                let f2 = Unique::generate('f');
                MExpr::Load {
                    bind: f2,
                    arg1: func,
                    index: 0,
                    cont: Box::new(MExpr::Call {
                        bind,
                        func: Atom::Var(f2),
                        args: {
                            let mut args = args;
                            args.insert(0, func);
                            args
                        },
                        cont,
                    }),
                }
            }
            MExpr::Retn { arg1 } => {
                let arg1 = self.visit_arg(arg1);
                MExpr::Retn { arg1 }
            }
            other => other
                .walk_cont(|cont| self.visit_expr(cont))
                .walk_bind(|bind| self.visit_bind(bind))
                .walk_brch(|brch| self.visit_expr(brch))
                .walk_arg(|arg| self.visit_arg(arg)),
        }
    }
}

#[test]
fn clos_conv_test() {
    use crate::anf::anf_build::*;

    // test free varible in function declaration
    let expr1 = chain(vec![
        iadd("v", i(1), i(2)),
        let_in(
            vec![fun(
                "f1",
                vec!["x1"],
                chain(vec![iadd("r", v("x1"), v("v")), retn(v("r"))]),
            )],
            vec![call("r", "f1", vec![i(42)]), retn(v("r"))],
        ),
    ]);
    let expr1 = ClosConv::run(expr1);
    let expr2 = let_in(
        vec![fun(
            "f1",
            vec!["c1", "x1"],
            chain(vec![
                offset("_f", v("c1"), 0),
                load("v2", v("c1"), 1),
                iadd("r", v("x1"), v("v2")),
                retn(v("r")),
            ]),
        )],
        vec![
            iadd("v1", i(1), i(2)),
            alloc("c2", 2),
            store(v("c2"), 1, v("v1")),
            store(v("c2"), 0, v("f1")),
            offset("f1c", v("c2"), 0),
            load("t", v("f1c"), 0),
            call("r", "t", vec![v("f1c"), i(42)]),
            retn(v("r")),
        ],
    );
    assert_eq!(expr1, expr2);

    // test mutual recursive declaration
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
        ],
        vec![call("r3", "f1", vec![i(42)]), retn(v("r3"))],
    );
    let expr1 = ClosConv::run(expr1);
    let expr2 = let_in(
        vec![
            fun(
                "f1",
                vec!["c1", "x1"],
                chain(vec![
                    offset("f2_1", v("c1"), 1),
                    offset("f1_1", v("c1"), 0),
                    load("t1", v("f2_1"), 0),
                    call("r1", "t1", vec![v("f2_1"), v("x1")]),
                    retn(v("r1")),
                ]),
            ),
            fun(
                "f2",
                vec!["c2", "x2"],
                chain(vec![
                    offset("f2_2", v("c2"), 0),
                    offset("f1_2", v("c2"), -1),
                    load("t2", v("f1_2"), 0),
                    call("r2", "t2", vec![v("f1_2"), v("x2")]),
                    retn(v("r2")),
                ]),
            ),
        ],
        vec![
            alloc("c3", 2),
            store(v("c3"), 1, v("f2")),
            store(v("c3"), 0, v("f1")),
            offset("f2c", v("c3"), 1),
            offset("f1c", v("c3"), 0),
            load("t", v("f1c"), 0),
            call("r3", "t", vec![v("f1c"), i(42)]),
            retn(v("r3")),
        ],
    );
    assert_eq!(expr1, expr2);

    // test nested function declaration
    let expr1 = let_in(
        vec![fun(
            "f1",
            vec!["x"],
            let_in(
                vec![fun(
                    "f2",
                    vec!["y"],
                    chain(vec![iadd("r", v("x"), v("y")), retn(v("r"))]),
                )],
                vec![retn(v("f2"))],
            ),
        )],
        vec![
            call("r1", "f1", vec![i(1)]),
            call("r2", "r1", vec![i(2)]),
            retn(v("r2")),
        ],
    );
    let expr1 = ClosConv::run(expr1);
    let expr2 = let_in(
        vec![
            fun(
                "f2",
                vec!["c2", "y2"],
                chain(vec![
                    offset("_f2", v("c2"), 0),
                    load("x2", v("c2"), 1),
                    iadd("r2", v("x2"), v("y2")),
                    retn(v("r2")),
                ]),
            ),
            fun(
                "f1",
                vec!["c1", "x1"],
                chain(vec![
                    offset("_f1", v("c1"), 0),
                    alloc("c", 2),
                    store(v("c"), 1, v("x1")),
                    store(v("c"), 0, v("f2")),
                    offset("r1", v("c"), 0),
                    retn(v("r1")),
                ]),
            ),
        ],
        vec![
            alloc("c3", 1),
            store(v("c3"), 0, v("f1")),
            offset("f1c", v("c3"), 0),
            load("f1t", v("f1c"), 0),
            call("f2c", "f1t", vec![v("f1c"), i(1)]),
            load("f2t", v("f2c"), 0),
            call("r3", "f2t", vec![v("f2c"), i(2)]),
            retn(v("r3")),
        ],
    );
    assert_eq!(expr1, expr2);
}
