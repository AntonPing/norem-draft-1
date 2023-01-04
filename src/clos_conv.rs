use std::collections::{HashMap, HashSet};
use std::mem;

use itertools::Itertools;

use crate::anf::*;
use crate::intern::Unique;

pub struct ClosConv {
    toplevel: Vec<MDecl>,
    free: HashSet<Unique>,
}

impl ClosConv {
    pub fn new() -> ClosConv {
        ClosConv {
            toplevel: Vec::new(),
            free: HashSet::new(),
        }
    }
    pub fn run(expr: MExpr) -> MExpr {
        let mut pass = ClosConv::new();
        let expr = pass.visit_expr(expr);
        MExpr::LetIn {
            decls: pass.toplevel,
            cont: Box::new(expr),
        }
    }

    fn listen_free<T, F>(&mut self, func: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        let mut temp = HashSet::new();
        mem::swap(&mut temp, &mut self.free);
        let result = func(self);
        mem::swap(&mut temp, &mut self.free);
        self.free.extend(temp.into_iter());
        result
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
                        foo1(v1,...,vn,x,y,...,z) =
                            bar;
                        foo2(c,x,y,...,z) =
                            let v1 = load(c,1);
                            ...
                            let vn = load(c,n);
                            foo1(v1,...,vn,x,y,...,z);
                    in
                        let foo3 = alloc(n)
                        store(foo3,0,foo2)
                        store(foo3,1,v1)
                        store(foo3,2,v2)
                        ...
                        store(foo3,n,vn)
                        baz[foo:=foo3]
                    end
                    where foo1, foo2, foo3 and c are generated variables
                */

                // record the order of function definition
                let func_names: Vec<_> = decls.iter().map(|decl| decl.func).collect();

                // transform function and get their free variable set
                let decls_frees: Vec<(_, Vec<_>)> = decls
                    .into_iter()
                    .map(|decl| {
                        self.listen_free(|slf| {
                            let decl = slf.visit_decl(decl);
                            let free = slf.free.iter().sorted().copied().collect();
                            (decl, free)
                        })
                    })
                    .collect();

                for func in &func_names {
                    self.free.remove(func);
                }

                // add function name to free var, although they are not
                let freevars: Vec<Unique> = func_names
                    .iter()
                    .chain(self.free.iter().sorted())
                    .copied()
                    .collect();

                // instead of returning the let-block: CExpr::Let { decls, cont }
                // we lift all decls to toplevel

                let mut func_map: HashMap<Unique, Unique> = HashMap::new();
                for (decl, free) in decls_frees {
                    /*
                    foo1(v1,...,vn,x,y,...,z) =
                        bar;
                    */
                    let func1 = decl.func.rename();
                    let pars1 = free.iter().chain(decl.pars.iter()).copied().collect();
                    let body1 = decl.body;
                    let decl1 = MDecl {
                        func: func1,
                        pars: pars1,
                        body: body1,
                    };
                    self.toplevel.push(decl1);

                    /*
                    foo2(c,x',y',...,z') =
                        let v1' = load(c,1);
                        ...
                        let vn' = load(c,n);
                        foo1(v1',...,vn',x',y',...,z');
                    */
                    let new_pars: Vec<_> = decl.pars.iter().map(|x| x.rename()).collect();
                    let new_free: Vec<_> = free.iter().map(|x| x.rename()).collect();
                    let c = Unique::generate('c');
                    let func2 = decl.func.rename();
                    let pars2 = [c].into_iter().chain(new_pars.iter().copied()).collect();
                    let args = new_free
                        .iter()
                        .cloned()
                        .chain(new_pars.iter().copied())
                        .map(|x| x.into())
                        .collect();
                    let body2 = freevars
                        .iter()
                        .enumerate()
                        .filter(|(_, x)| free.contains(x))
                        .zip(new_free.iter())
                        .map(|((i, _), x)| (i, x))
                        .fold(MExpr::make_tail_call(func1, args), |cont, (i, x)| {
                            MExpr::Stmt {
                                bind: Some(*x),
                                prim: StmtPrim::Load,
                                args: vec![Atom::Var(c), Atom::Int(i as i64)],
                                cont: Box::new(cont),
                            }
                        });
                    let decl2 = MDecl {
                        func: func2,
                        pars: pars2,
                        body: body2,
                    };
                    self.toplevel.push(decl2);
                    func_map.insert(decl.func, func2);
                }

                // transform let-block continuation
                let cont = self.listen_free(|slf| {
                    let cont = slf.visit_expr(*cont);
                    cont
                });
                for func in &func_names {
                    self.free.remove(func);
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

                // generate shared closure 'c'
                let c = Unique::generate('c');

                // here is the (function offset) part
                let cont = func_names
                    .iter()
                    .enumerate()
                    .fold(cont, |expr, (i, f)| MExpr::Stmt {
                        bind: Some(*f),
                        prim: StmtPrim::Offset,
                        args: vec![Atom::Var(c), Atom::Int(i as i64)],
                        cont: Box::new(expr),
                    });

                // here is the (block functions) and (free variables) part
                let cont = freevars
                    .iter()
                    .enumerate()
                    .fold(cont, |expr, (i, x)| MExpr::Stmt {
                        bind: None,
                        prim: StmtPrim::Store,
                        args: vec![
                            Atom::Var(c),
                            Atom::Int(i as i64),
                            Atom::Var(*func_map.get(x).unwrap_or(x)),
                        ],
                        cont: Box::new(expr),
                    });

                // here is the (record creation) part
                let cont = MExpr::Stmt {
                    bind: Some(c),
                    prim: StmtPrim::Alloc,
                    args: vec![Atom::Int(freevars.len() as i64)],
                    cont: Box::new(cont),
                };

                cont
            }
            MExpr::Stmt {
                bind,
                prim,
                args,
                cont,
            } => {
                let cont = Box::new(self.visit_expr(*cont));
                if let Some(x) = bind {
                    self.free.remove(&x);
                }
                let args: Vec<Atom> = args.into_iter().map(|arg| self.visit_atom(arg)).collect();
                MExpr::Stmt {
                    prim,
                    args,
                    bind,
                    cont,
                }
            }
            MExpr::Brch { prim, args, conts } => {
                let conts = conts
                    .into_iter()
                    .map(|cont| self.visit_expr(cont))
                    .collect();
                let args: Vec<Atom> = args.into_iter().map(|arg| self.visit_atom(arg)).collect();
                MExpr::Brch { prim, args, conts }
            }
            MExpr::Call {
                bind,
                func: CallFunc::Intern(f),
                args,
                cont,
            } => {
                /*
                    let bind = func(a,b,...,z);
                    cont;
                    =====> becomes =====>
                    let f = load(func,0);
                    let bind = f(func,a,b,...,z);
                    cont;
                */
                let f2 = Unique::generate('f');
                let cont = Box::new(self.visit_expr(*cont));
                if let Some(x) = bind {
                    self.free.remove(&x);
                }
                self.free.insert(f);
                let args: Vec<_> = args.into_iter().map(|arg| self.visit_atom(arg)).collect();
                MExpr::Stmt {
                    bind: Some(f2),
                    prim: StmtPrim::Load,
                    args: vec![Atom::Var(f), Atom::Int(0)],
                    cont: Box::new(MExpr::Call {
                        func: CallFunc::Intern(f2),
                        args: {
                            let mut args = args;
                            args.insert(0, Atom::Var(f));
                            args
                        },
                        bind,
                        cont,
                    }),
                }
            }
            MExpr::Call {
                bind,
                func: CallFunc::Extern(f),
                args,
                cont,
            } => {
                let cont = Box::new(self.visit_expr(*cont));
                if let Some(x) = bind {
                    self.free.remove(&x);
                }
                let args = args.into_iter().map(|arg| self.visit_atom(arg)).collect();
                MExpr::Call {
                    func: CallFunc::Extern(f),
                    args,
                    bind,
                    cont,
                }
            }
            MExpr::Retn { atom } => {
                let atom = self.visit_atom(atom);
                MExpr::Retn { atom }
            }
        }
    }

    fn visit_atom(&mut self, atom: Atom) -> Atom {
        if let Atom::Var(sym) = atom {
            self.free.insert(sym);
        }
        atom
    }

    fn visit_decl(&mut self, decl: MDecl) -> MDecl {
        let MDecl { func, pars, body } = decl;
        let body = self.visit_expr(body);
        for par in pars.iter() {
            self.free.remove(par);
        }
        MDecl { func, pars, body }
    }
}

#[test]
#[allow(unused_imports)]
fn clos_conv_test() {
    use crate::anf::BrchPrim::*;
    use crate::anf::StmtPrim::*;
    use crate::parser::*;
    use crate::renamer::Renamer;
    use crate::{atom, decls, expr, ident};
    let expr1 = expr! {
        letin [
            fun f1 (x1) => {
                retn x1;
            }
        ] {
            call r = f1 (42);
            retn r;
        }
    };

    let expr1 = ClosConv::run(expr1);
    let expr2 = expr! {
        letin [
            fun f1_1 (x1_1) => {
                retn x1_1;
            };
            fun f1_2 (c1,x1_2) => {
                call r1 = f1_1 (x1_2);
                retn r1;
            }
        ] {
            stmt c2 = Alloc, 1;
            stmt Store, c2, 0, f1_2;
            stmt c3 = Offset, c2, 0;
            stmt f2 = Load, c3, 0;
            call r2 = f2 (c3, 42);
            retn r2;
        }
    };
    assert_eq!(expr1, expr2);

    /*
    todo: more complicated tests

    let expr1 = expr! {
        letin [
            fun f1 (x1) => {
                letin [
                    fun f2 (x2) => {
                        stmt x3 = IAdd, x1, x2;
                        retn x3;
                    }
                ] {
                    retn f2;
                }
            }
        ] {
            call f3 = f1 (1);
            call res = f3 (2);
            retn res;
        }
    };
    let expr1 = ClosConv::run(expr1);

    let expr2 = expr! {
        letin [
            fun f2_1 (x1,x2) => {
                stmt x3 = IAdd, x1, x2;
                retn x3;
            };
            fun f2_2 (c1,x3) => {
                stmt x4 = Load, c1, 1;
                call x5 = f2_1 (x3, x4);
                retn x5;
            };
            fun f1_1 (x6) => {
                stmt c2 = Alloc, 2;
                stmt Store, c2, 1, x6;
                stmt Store, c2, 0, f2_2;
                stmt r1 = Offset, c2, 0;
                retn r1;
            };
            fun f1_2 (c3,x6) => {
                call r2 = f1_1 (x6);
                retn r2;
            }
        ] {
            stmt c4 = Alloc, 1;
            stmt Store, c4, 0, f1_2;
            stmt f3 = Offset, c4, 0;
            call f4 = f3 (1);
            ...
        }
    };
    */
}
