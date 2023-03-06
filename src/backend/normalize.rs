use super::patn_mat::PatnMatrix;
use super::*;
use crate::frontend::ast::*;
use crate::frontend::context::Context;
pub struct Normalize<'a> {
    ctx: &'a Context,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ColType {
    Any,
    Data(Ident),
    Lit(LitType),
}

impl<'a> Normalize<'a> {
    pub fn new(ctx: &'a Context) -> Normalize<'a> {
        Normalize { ctx }
    }
    pub fn run(expr: &Expr, ctx: &'a Context) -> MExpr {
        let mut pass = Normalize::new(ctx);
        pass.normalize_top(expr)
    }

    fn normalize_top(&mut self, expr: &Expr) -> MExpr {
        let bind = Ident::generate('r');
        self.normalize(
            expr,
            bind,
            MExpr::Retn {
                arg1: Atom::Var(bind),
            },
        )
    }

    // translate from Expr to MExpr, basically a lowering pass
    // order of evaluation for function arguments: from right to left
    fn normalize(&mut self, expr: &Expr, hole: Ident, rest: MExpr) -> MExpr {
        match expr {
            Expr::Lit { lit, .. } => MExpr::UnOp {
                bind: hole,
                prim: UnOpPrim::Move,
                arg1: (*lit).into(),
                cont: Box::new(rest),
            },
            Expr::Var { var, .. } => MExpr::UnOp {
                bind: hole,
                prim: UnOpPrim::Move,
                arg1: Atom::Var(*var),
                cont: Box::new(rest),
            },
            Expr::Prim { prim, args, .. } => {
                // normalize(@prim(e1,e2), hole, rest) =
                // normalize(e2,x2,normalize(e1,x1, let hole = prim(x1,x2) in rest))
                let tempvars: Vec<Ident> = args.iter().map(|_| Ident::generate('x')).collect();
                pub enum OpPrim {
                    Unary(UnOpPrim),
                    Binary(BinOpPrim),
                }

                let prim = match prim {
                    Builtin::IAdd => OpPrim::Binary(BinOpPrim::IAdd),
                    Builtin::ISub => OpPrim::Binary(BinOpPrim::ISub),
                    Builtin::IMul => OpPrim::Binary(BinOpPrim::IMul),
                    Builtin::IDiv => todo!(),
                    Builtin::IRem => todo!(),
                    Builtin::INeg => OpPrim::Unary(UnOpPrim::INeg),
                    Builtin::RAdd => todo!(),
                    Builtin::RSub => todo!(),
                    Builtin::RMul => todo!(),
                    Builtin::RDiv => todo!(),
                    Builtin::BAnd => todo!(),
                    Builtin::BOr => todo!(),
                    Builtin::BNot => todo!(),
                    Builtin::ICmpGr => OpPrim::Binary(BinOpPrim::ICmpGr),
                    Builtin::ICmpLs => OpPrim::Binary(BinOpPrim::ICmpLs),
                    Builtin::ICmpEq => OpPrim::Binary(BinOpPrim::ICmpEq),
                    Builtin::ICmpLe => OpPrim::Binary(BinOpPrim::ICmpLe),
                    Builtin::ICmpGe => OpPrim::Binary(BinOpPrim::ICmpGe),
                    Builtin::ICmpNe => OpPrim::Binary(BinOpPrim::ICmpNe),
                    Builtin::RCmpGr => todo!(),
                    Builtin::RCmpLs => todo!(),
                    Builtin::RCmpEq => todo!(),
                    Builtin::RCmpLe => todo!(),
                    Builtin::RCmpGe => todo!(),
                    Builtin::RCmpNe => todo!(),
                };

                let stmt = match prim {
                    OpPrim::Unary(prim) => {
                        assert!(args.len() == 1);
                        MExpr::UnOp {
                            bind: hole,
                            prim,
                            arg1: Atom::Var(tempvars[0]),
                            cont: Box::new(rest),
                        }
                    }
                    OpPrim::Binary(prim) => {
                        assert!(args.len() == 2);
                        MExpr::BinOp {
                            bind: hole,
                            prim,
                            arg1: Atom::Var(tempvars[0]),
                            arg2: Atom::Var(tempvars[1]),
                            cont: Box::new(rest),
                        }
                    }
                };

                tempvars
                    .into_iter()
                    .zip(args.iter())
                    .fold(stmt, |res, (bind, arg)| self.normalize(arg, bind, res))
            }
            Expr::Fun { pars, body, .. } => {
                // normalize(fun(x1,x2,...) => e, hole, ctx) =
                // let f(x1,x2,...) = normalize_top(e) in ctx[hole:=f]
                let funcvar = Ident::generate('f');
                MExpr::LetIn {
                    decls: vec![MDecl {
                        func: funcvar,
                        pars: pars.clone(),
                        body: self.normalize_top(body),
                    }],
                    cont: Box::new(MExpr::UnOp {
                        bind: hole,
                        prim: UnOpPrim::Move,
                        arg1: Atom::Var(funcvar),
                        cont: Box::new(rest),
                    }),
                }
            }
            Expr::App { func, args, .. } => {
                /*  normalize(e0(e1,..,en), hole, ctx) =
                    normalize(en,xn,
                        ...
                            normalize(e1,x1,
                                normalize(e0,f,
                                    let hole = f(x1,...,xn) in ctx))...)
                */
                let funcvar = Ident::generate('f');
                let argvars: Vec<Ident> = args.iter().map(|_| Ident::generate('x')).collect();
                let res = MExpr::Call {
                    bind: hole,
                    func: Atom::Var(funcvar),
                    args: argvars.iter().map(|arg| Atom::Var(*arg)).collect(),
                    cont: Box::new(rest),
                };
                let res = self.normalize(func, funcvar, res);
                let res = argvars
                    .iter()
                    .cloned()
                    .zip(args.into_iter())
                    .fold(res, |res, (bind, arg)| self.normalize(arg, bind, res));
                res
            }
            Expr::ExtCall { func, args, .. } => {
                /*  normalize(f(e1,..,en), hole, rest) =
                        normalize(en,xn,
                            ...
                                normalize(e1,x1,
                                    let hole = f(x1,...,xn) in rest))...)
                */
                let argvars: Vec<Ident> = args.iter().map(|_| Ident::generate('x')).collect();
                let res = MExpr::ExtCall {
                    bind: hole,
                    func: *func,
                    args: argvars.iter().map(|arg| Atom::Var(*arg)).collect(),
                    cont: Box::new(rest),
                };
                let res = argvars
                    .iter()
                    .cloned()
                    .zip(args.into_iter())
                    .fold(res, |res, (bind, arg)| self.normalize(arg, bind, res));
                res
            }
            Expr::Cons { cons, args, .. } => {
                /*
                normalize(ci(e1,..,en), hole, rest) =
                normalize(en,xn,
                    ...
                        normalize(e1,x1,
                            normalize(e0,f,
                                let m = alloc(n);
                                store m[0] = i;
                                store m[1] = x1;
                                ......
                                store m[n] = xn;
                                let hole = move(m);
                                rest
                ))...)
                */
                let m = Ident::generate('m');
                let argvars: Vec<Ident> = args.iter().map(|_| Ident::generate('x')).collect();
                let res = MExpr::UnOp {
                    bind: hole,
                    prim: UnOpPrim::Move,
                    arg1: Atom::Var(m),
                    cont: Box::new(rest),
                };

                let res = argvars
                    .iter()
                    .enumerate()
                    .fold(res, |cont, (i, x)| MExpr::Store {
                        arg1: Atom::Var(m),
                        index: i + 1,
                        arg2: Atom::Var(*x),
                        cont: Box::new(cont),
                    });

                let res = MExpr::Store {
                    arg1: Atom::Var(m),
                    index: 0,
                    arg2: Atom::Int(self.ctx.get_cons_index(cons) as i64),
                    cont: Box::new(res),
                };

                let arity = self.ctx.cons_env[cons].flds.len();

                let res = MExpr::Alloc {
                    bind: m,
                    size: arity + 1,
                    cont: Box::new(res),
                };

                let res = argvars
                    .iter()
                    .cloned()
                    .zip(args.into_iter())
                    .fold(res, |res, (bind, arg)| self.normalize(arg, bind, res));

                res
            }
            Expr::Begin { block, .. } => {
                /*
                    normalize(
                        begin
                            let x1 = e1;
                            ...
                            let xn = en;
                            res
                        end,
                        hole,
                        rest
                    ) =

                    normalize(e1,x1,
                        normalize(e2,x2,
                            ...
                            normalize(en,xn,)
                                normalize(res,hole,rest)
                    ))...)
                */
                self.normalize_block(block, hole, rest)
            }
            Expr::Case { expr, rules, .. } => {
                /*
                    normalize(
                        case etop of
                        | pattern_1 => e_0
                        ......
                        | pattern_n => e_n-1
                    hole,
                    rest
                    ) =

                    normalize(etop, o
                        block
                            a_0(..) = e_0;
                            ......
                            a_n-1(..) = e_n;
                        in
                            compile_match_with(
                                (o),
                                (pattern_1)
                                ......
                                (pattern_n),
                                (a_1(..),......,a_n-1(..)),
                                hole,
                                rest
                            )
                        end
                    )
                */
                let etop = Ident::generate('o');
                let objs = vec![etop];
                let mut decls: Vec<MDecl> = Vec::new();
                let (matrix, acts): (Vec<Vec<_>>, Vec<_>) = rules
                    .into_iter()
                    .map(|rule| {
                        let Rule { patn, body, .. } = rule;
                        let func = Ident::generate('a');
                        let pars = patn.get_freevars();
                        let args = pars.clone();
                        let decl = MDecl {
                            func,
                            pars,
                            body: self.normalize_top(body),
                        };
                        decls.push(decl);
                        let act = (func, args);
                        (vec![patn.clone()], act)
                    })
                    .unzip();

                let mat = PatnMatrix { objs, matrix, acts };
                let cont = Box::new(self.compile_match(&mat, hole, rest));
                self.normalize(expr, etop, MExpr::LetIn { decls, cont })
            }
            Expr::Ifte {
                cond, trbr, flbr, ..
            } => {
                /*
                    normalize(
                        if cond then trbr else flbr,
                        hole,
                        rest
                    ) =
                    normalize(cond, x,
                        let hole = if x
                            then normalize_top(trbr)
                            else normalize_top(flbr);
                        rest
                    )
                */
                let x = Ident::generate('x');
                let cont = MExpr::Ifte {
                    bind: hole,
                    arg1: Atom::Var(x),
                    brch1: Box::new(self.normalize_top(trbr)),
                    brch2: Box::new(self.normalize_top(flbr)),
                    cont: Box::new(rest),
                };
                self.normalize(&cond, x, cont)
            }
            Expr::Letrec { decls, block, .. } => {
                /*
                    normalize(
                        block
                            fun f1(x1,...,z1) = body1;
                            ......
                            fun fn(xn,...,zn) = bodyn;
                        in
                            cont
                        end,
                        hole,
                        ctx
                    ) =

                    block
                        fun f1(x1,...,z1) = normalize_top(body1);
                        ......
                        fun fn(xn,...,zn) = normalize_top(bodyn);
                    in
                        normalize(cont,hole,ctx)
                    end,
                */
                let decls = decls
                    .into_iter()
                    .filter_map(|decl| match decl {
                        Decl::Func {
                            name, pars, body, ..
                        } => Some(MDecl {
                            func: *name,
                            pars: pars.iter().map(|(par, _)| *par).collect(),
                            body: self.normalize_top(body),
                        }),
                        Decl::Data { .. } => None,
                        Decl::Type { .. } => None,
                        Decl::Extern { .. } => None,
                    })
                    .collect();
                let cont = Box::new(self.normalize_block(block, hole, rest));
                MExpr::LetIn { decls, cont }
            }
        }
    }

    fn normalize_block(&mut self, block: &Block, hole: Ident, ctx: MExpr) -> MExpr {
        let Block { stmts, retn, .. } = block;
        let res = if let Some(retn) = retn {
            self.normalize(retn, hole, ctx)
        } else {
            MExpr::UnOp {
                bind: hole,
                prim: UnOpPrim::Move,
                arg1: Atom::Unit,
                cont: Box::new(ctx),
            }
        };
        stmts.into_iter().rev().fold(res, |cont, stmt| match stmt {
            Stmt::Bind { bind, expr, .. } => self.normalize(expr, *bind, cont),
            Stmt::Do { expr, .. } => {
                let bind = Ident::generate('_');
                self.normalize(expr, bind, cont)
            }
        })
    }

    fn compile_match_top(&mut self, mat: &PatnMatrix) -> MExpr {
        let r = Ident::generate('r');
        self.compile_match(mat, r, MExpr::Retn { arg1: Atom::Var(r) })
    }

    fn compile_match(&mut self, mat: &PatnMatrix, hole: Ident, rest: MExpr) -> MExpr {
        if mat.is_empty() {
            panic!("pattern match not exhaustive!")
        } else if mat.first_row_aways_match() {
            let (bindings, func, args) = mat.success();
            bindings.into_iter().fold(
                MExpr::Call {
                    bind: hole,
                    func: Atom::Var(func),
                    args: args.into_iter().map(|x| Atom::Var(x)).collect(),
                    cont: Box::new(rest),
                },
                |cont, (var, obj)| MExpr::UnOp {
                    bind: var,
                    prim: UnOpPrim::Move,
                    arg1: Atom::Var(obj),
                    cont: Box::new(cont),
                },
            )
        } else {
            let j = self.get_best_col();
            match self.get_col_type(mat, j) {
                ColType::Any => self.match_default(mat, j),
                ColType::Data(data) => {
                    let cons_set = mat.get_cons_set(j);
                    let mut exhaustive = true;
                    let brchs = self.ctx.data_env[&data]
                        .cons
                        .clone()
                        .into_iter()
                        .enumerate()
                        .flat_map(|(i, cons)| {
                            if cons_set.contains(&cons) {
                                let res = self.match_specialize_cons(mat, j, &cons);
                                Some((i, res))
                            } else {
                                exhaustive = false;
                                None
                            }
                        })
                        .collect();
                    let dflt = if exhaustive {
                        None
                    } else {
                        Some(Box::new(self.match_default(mat, j)))
                    };
                    let t = Ident::generate('t');
                    MExpr::Load {
                        bind: t,
                        arg1: Atom::Var(mat.objs[j]),
                        index: 0,
                        cont: Box::new(MExpr::Switch {
                            bind: hole,
                            arg1: Atom::Var(t),
                            brchs,
                            dflt,
                            cont: Box::new(rest),
                        }),
                    }
                }
                ColType::Lit(LitType::Real) => {
                    panic!("pattern match on real numbers are not allowed!");
                }
                ColType::Lit(LitType::Unit) => {
                    // no need to generating switch for Unit type
                    self.match_specialize_lit(mat, j, LitVal::Unit)
                }
                ColType::Lit(_) => {
                    let lit_set = mat.get_lit_set(j);
                    let brchs = lit_set
                        .into_iter()
                        .flat_map(|lit| {
                            let cont = self.match_specialize_lit(mat, j, lit);
                            match lit {
                                LitVal::Int(x) => Some((x as usize, cont)),
                                LitVal::Real(_) => unreachable!(),
                                LitVal::Bool(x) => Some((x as usize, cont)),
                                LitVal::Char(x) => Some((x as usize, cont)),
                                LitVal::Unit => unreachable!(),
                            }
                        })
                        .collect();
                    let dflt = Some(Box::new(self.match_default(mat, j)));
                    MExpr::Switch {
                        bind: hole,
                        arg1: Atom::Var(mat.objs[j]),
                        brchs,
                        dflt,
                        cont: Box::new(rest),
                    }
                }
            }
        }
    }

    fn match_specialize_lit(&mut self, mat: &PatnMatrix, j: usize, lit: LitVal) -> MExpr {
        let (new_mat, bindings) = mat.specialize_lit(j, lit);
        let cont = self.compile_match_top(&new_mat);
        bindings
            .into_iter()
            .fold(cont, |cont, (var, obj)| MExpr::UnOp {
                bind: var,
                prim: UnOpPrim::Move,
                arg1: Atom::Var(obj),
                cont: Box::new(cont),
            })
    }

    fn match_specialize_cons(&mut self, mat: &PatnMatrix, j: usize, cons: &Ident) -> MExpr {
        let arity = self.ctx.cons_env[cons].flds.len();
        let (new_mat, new_objs, bindings) = mat.specialize_cons(j, cons, arity);
        let cont = self.compile_match_top(&new_mat);
        let cont = new_objs
            .into_iter()
            .enumerate()
            .fold(cont, |cont, (i, obj)| MExpr::Load {
                bind: obj,
                arg1: Atom::Var(mat.objs[j]),
                index: i + 1,
                cont: Box::new(cont),
            });
        bindings
            .into_iter()
            .fold(cont, |cont, (var, obj)| MExpr::UnOp {
                bind: var,
                prim: UnOpPrim::Move,
                arg1: Atom::Var(obj),
                cont: Box::new(cont),
            })
    }

    fn match_default(&mut self, mat: &PatnMatrix, j: usize) -> MExpr {
        let (new_mat, bindings) = mat.default(j);
        let cont = self.compile_match_top(&new_mat);
        bindings
            .into_iter()
            .fold(cont, |cont, (var, obj)| MExpr::UnOp {
                bind: var,
                prim: UnOpPrim::Move,
                arg1: Atom::Var(obj),
                cont: Box::new(cont),
            })
    }

    fn get_best_col(&self) -> usize {
        // todo: better hueristic
        0
    }

    fn get_col_type(&mut self, mat: &PatnMatrix, j: usize) -> ColType {
        let mut res = ColType::Any;
        for row in mat.matrix.iter() {
            match &row[j] {
                Pattern::Var { .. } => {}
                Pattern::Lit { lit, .. } => match res {
                    ColType::Any => res = ColType::Lit(lit.get_lit_type()),
                    ColType::Lit(lit2) => {
                        if lit2 != lit.get_lit_type() {
                            panic!("pattern match not well-typed!")
                        }
                    }
                    ColType::Data(_) => {
                        panic!("pattern match not well-typed!")
                    }
                },
                Pattern::Cons { cons, .. } => match res {
                    ColType::Any => res = ColType::Data(self.ctx.cons_env[cons].data),
                    ColType::Lit(_) => {
                        panic!("pattern match not well-typed!")
                    }
                    ColType::Data(data) => {
                        if data != self.ctx.cons_env[cons].data {
                            panic!("pattern match not well-typed!")
                        }
                    }
                },
                Pattern::Wild { .. } => {}
            }
        }
        res
    }
}

#[test]
fn normalize_test() {
    use super::anf_build::*;
    use crate::frontend::infer::Infer;
    use crate::frontend::parser::*;
    use crate::frontend::renamer::Renamer;

    let string = r#"
@iadd(@iadd(1,2),@iadd(3,4))
    "#;
    let mut par = Parser::new(string);
    let mut expr1 = parse_expr(&mut par).unwrap();
    Renamer::run(&mut expr1).unwrap();
    let ctx = Infer::run(&expr1).unwrap();
    let expr1 = Normalize::run(&expr1, &ctx);
    let expr2 = chain(vec![
        _move("x1", i(4)),
        _move("x2", i(3)),
        iadd("x3", v("x2"), v("x1")),
        _move("x4", i(2)),
        _move("x5", i(1)),
        iadd("x6", v("x5"), v("x4")),
        iadd("x7", v("x6"), v("x3")),
        retn(v("x7")),
    ]);
    // println!("{expr1}");
    // println!("{expr2}");
    assert_eq!(expr1, expr2);

    let string = r#"
    begin
        let f = fn(x) => @iadd(x,1);
        f(42)
    end
    "#;
    let mut par = Parser::new(string);
    let mut expr1 = parse_expr(&mut par).unwrap();
    Renamer::run(&mut expr1).unwrap();
    let ctx = Infer::run(&expr1).unwrap();
    let expr1 = Normalize::run(&expr1, &ctx);
    let expr2 = let_in(
        vec![fun(
            "f1",
            vec!["x1"],
            chain(vec![
                _move("x2", i(1)),
                _move("x3", v("x1")),
                iadd("x4", v("x3"), v("x2")),
                retn(v("x4")),
            ]),
        )],
        vec![
            _move("f2", v("f1")),
            _move("x5", i(42)),
            _move("f3", v("f2")),
            call("x6", "f3", vec![v("x5")]),
            retn(v("x6")),
        ],
    );
    // println!("{expr1}");
    // println!("{expr2}");
    assert_eq!(expr1, expr2);
}

#[test]
fn normalize_pattern_match_test() {
    use crate::frontend::infer::Infer;
    use crate::frontend::parser::*;
    use crate::frontend::renamer::Renamer;
    let string = r#"
letrec
    data List[T] =
    | Cons(T,List[T])
    | Nil
    end
    func length[T](lst: List[T]): Int =
        case lst of
        | Cons(head,tail) => @iadd(1, length(tail))
        | Nil => 0
        end
in
    length(Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil))))))
end
"#;
    let mut par = Parser::new(string);
    let mut expr1 = parse_expr(&mut par).unwrap();
    Renamer::run(&mut expr1).unwrap();
    println!("{expr1:#?}");
    let ctx = Infer::run(&expr1).unwrap();
    let expr1 = Normalize::run(&expr1, &ctx);
    println!("{expr1}");
}
