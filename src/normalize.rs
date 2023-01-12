use crate::anf::*;
use crate::ast::*;
use crate::intern::Unique;

pub fn subst(expr: MExpr, hole: Unique, atom: Atom) -> MExpr {
    // subst(expr,hole,atom) ~=~ let hole = move(atom); expr
    // it will be substituted in constant-fold pass anyway
    MExpr::Stmt {
        bind: Some(hole),
        stmt: MStmt::Move { arg1: atom },
        cont: Box::new(expr),
    }
}

pub fn normalize_expr(expr: &Expr<Unique>) -> MExpr {
    let bind = Unique::generate('r');
    normalize_expr_aux(
        expr,
        bind,
        MExpr::Retn {
            atom: Atom::Var(bind),
        },
    )
}

// translate from Expr<Unique> to MExpr, basically a lowering pass
// order of evaluation for function arguments: from right to left
fn normalize_expr_aux(expr: &Expr<Unique>, hole: Unique, ctx: MExpr) -> MExpr {
    match expr {
        Expr::Lit { lit, .. } => subst(ctx, hole, (*lit).into()),
        Expr::Var { var, .. } => subst(ctx, hole, Atom::Var(*var)),
        Expr::Prim { prim, args, .. } => {
            // normalize(@iadd(e1,e2), hole, ctx) =
            // normalize(e2,x2,normalize(e1,x1, let hole = iadd(x1,x2) in ctx))
            let tempvars: Vec<Unique> = args.iter().map(|_| Unique::generate('x')).collect();

            let stmt = match prim {
                Builtin::IAdd => {
                    assert_eq!(args.len(), 2);
                    MStmt::IAdd {
                        arg1: Atom::Var(tempvars[0]),
                        arg2: Atom::Var(tempvars[1]),
                    }
                }
                Builtin::ISub => {
                    assert_eq!(args.len(), 2);
                    MStmt::ISub {
                        arg1: Atom::Var(tempvars[0]),
                        arg2: Atom::Var(tempvars[1]),
                    }
                }
                Builtin::IMul => {
                    assert_eq!(args.len(), 2);
                    MStmt::IMul {
                        arg1: Atom::Var(tempvars[0]),
                        arg2: Atom::Var(tempvars[1]),
                    }
                }
                Builtin::IDiv => todo!(),
                Builtin::IRem => todo!(),
                Builtin::INeg => todo!(),
                Builtin::RAdd => todo!(),
                Builtin::RSub => todo!(),
                Builtin::RMul => todo!(),
                Builtin::RDiv => todo!(),
                Builtin::BAnd => todo!(),
                Builtin::BOr => todo!(),
                Builtin::BNot => todo!(),
            };
            tempvars.into_iter().zip(args.iter()).fold(
                MExpr::Stmt {
                    bind: Some(hole),
                    stmt,
                    cont: Box::new(ctx),
                },
                |res, (bind, arg)| normalize_expr_aux(arg, bind, res),
            )
        }
        Expr::Fun { pars, body, .. } => {
            // normalize(fun(x,y) => e, hole, ctx) =
            // let f(x,y) = normalize_top(e) in ctx[hole:=f]
            let funcvar = Unique::generate('f');
            MExpr::LetIn {
                decls: vec![MDecl {
                    func: funcvar,
                    pars: pars.clone(),
                    body: normalize_expr(body),
                }],
                cont: Box::new(subst(ctx, hole, Atom::Var(funcvar))),
            }
        }
        Expr::App { func, args, .. } => {
            // normalize(e0(e1,..,en), hole, ctx) =
            // normalize(en,xn,
            //   ...
            //     normalize(e1,x1,
            //       normalize(e0,f,
            //         let hole = f(x1,...,xn) in ctx))...)
            let funcvar = Unique::generate('f');
            let argvars: Vec<Unique> = args.iter().map(|_| Unique::generate('x')).collect();
            let res = MExpr::Call {
                bind: Some(hole),
                func: CallFunc::Intern(funcvar),
                args: argvars.iter().map(|arg| Atom::Var(*arg)).collect(),
                cont: Box::new(ctx),
            };
            let res = normalize_expr_aux(func, funcvar, res);
            let res = argvars
                .iter()
                .cloned()
                .zip(args.into_iter())
                .fold(res, |res, (bind, arg)| normalize_expr_aux(arg, bind, res));
            res
        }
        Expr::Let {
            bind, expr, cont, ..
        } => {
            // normalize(let x = e1; e2, hole, ctx) =
            // normalize(e1,x,normalize(e2,hole,ctx))
            let res = normalize_expr_aux(cont, hole, ctx);
            let res = normalize_expr_aux(expr, *bind, res);
            res
        }
        Expr::Case {
            expr: _,
            rules: _,
            span: _,
        } => {
            todo!();
        }
        Expr::Blk {
            decls: _,
            cont: _,
            span: _,
        } => {
            todo!();
        }
    }
}

#[test]
fn normalize_test() {
    use crate::anf::anf_build::*;
    use crate::parser::*;
    use crate::renamer::Renamer;

    let string = r#"
@iadd(@iadd(1,2),@iadd(3,4))
    "#;
    let mut par = Parser::new(string);
    let expr1 = parse_expr(&mut par).unwrap();
    let mut rnm = Renamer::new();
    let expr1 = rnm.visit_expr(expr1);
    let expr1 = normalize_expr(&expr1);
    let expr2 = block(vec![
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
let f = fun(x) => @iadd(x,1);
f(42)
    "#;
    let mut par = Parser::new(string);
    let expr1 = parse_expr(&mut par).unwrap();
    let mut rnm = Renamer::new();
    let expr1 = rnm.visit_expr(expr1);
    let expr1 = normalize_expr(&expr1);
    let expr2 = letin_block(
        vec![fun(
            "f1",
            vec!["x1"],
            block(vec![
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
