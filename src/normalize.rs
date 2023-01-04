use crate::anf::*;
use crate::ast::*;
use crate::intern;
use crate::intern::Unique;

fn newvar() -> Unique {
    intern::intern("x").to_unique()
}

pub fn subst(expr: MExpr, hole: Unique, atom: Atom) -> MExpr {
    // subst(expr,hole,atom) ~=~ let hole = @move(atom); expr
    // it will be substituted in constant-fold pass anyway
    MExpr::Stmt {
        bind: Some(hole),
        prim: StmtPrim::Move,
        args: vec![atom],
        cont: Box::new(expr),
    }
}

pub fn normalize_expr(expr: &Expr<Unique>) -> MExpr {
    let bind = newvar();
    normalize_expr_aux(
        expr,
        bind,
        MExpr::Retn {
            atom: Atom::Var(bind),
        },
    )
}

// translate from Expr<Unique> to MExpr, basically lowering
// order of evaluation for function arguments: from right to left
fn normalize_expr_aux(expr: &Expr<Unique>, hole: Unique, ctx: MExpr) -> MExpr {
    match expr {
        Expr::Lit { lit, .. } => subst(ctx, hole, (*lit).into()),
        Expr::Var { var, .. } => subst(ctx, hole, Atom::Var(*var)),
        Expr::Prim { prim, args, .. } => {
            // normalize(@iadd(e1,e2), hole, ctx) =
            // normalize(e1,x1,normalize(e2,x2, let hole = iadd(x1,x2) in ctx))
            let tempvars: Vec<Unique> = args.iter().map(|_| newvar()).collect();
            let res = MExpr::Stmt {
                bind: Some(hole),
                prim: (*prim).into(),
                args: tempvars.iter().map(|arg| Atom::Var(*arg)).collect(),
                cont: Box::new(ctx),
            };
            let res = tempvars
                .iter()
                .zip(args.iter())
                .fold(res, |res, (bind, arg)| normalize_expr_aux(arg, *bind, res));
            res
        }
        Expr::Fun { pars, body, .. } => {
            // normalize(fun(x,y) => e, hole, ctx) =
            // let f(x,y) = normalize_top(e) in ctx[hole:=f]
            let funcvar = newvar();
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
            let funcvar = newvar();
            let argvars: Vec<Unique> = args.iter().map(|_| newvar()).collect();
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
            // normalize(let x = e1 in e2, hole, ctx) =
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
#[allow(unused_imports)]
fn normalize_test() {
    use crate::anf::BrchPrim::*;
    use crate::anf::StmtPrim::*;
    use crate::parser::*;
    use crate::renamer::Renamer;
    use crate::{atom, decls, expr, ident};

    let string = r#"
@iadd(@iadd(1,2),@iadd(3,4))
    "#;
    let mut par = Parser::new(string);
    let expr1 = parse_expr(&mut par).unwrap();
    let mut rnm = Renamer::new();
    let expr1 = rnm.visit_expr(expr1);
    let expr1 = normalize_expr(&expr1);
    // println!("{expr1}");
    let expr2 = expr! {
        stmt x1 = Move, 4;
        stmt x2 = Move, 3;
        stmt x3 = IAdd, x2, x1;
        stmt x4 = Move, 2;
        stmt x5 = Move, 1;
        stmt x6 = IAdd, x5, x4;
        stmt x7 = IAdd, x6, x3;
        retn x7;
    };
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
    // println!("{expr1}");
    let expr2 = expr! {
        letin [
            fun f1 (x1) => {
                stmt x2 = Move, 1;
                stmt x3 = Move, x1;
                stmt x4 = IAdd, x3, x2;
                retn x4;
            }
        ] {
            stmt f2 = Move, f1;
            stmt x5 = Move, 42;
            stmt x6 = Move, f2;
            call x7 = x6 (x5);
            retn x7;
        }
    };
    assert_eq!(expr1, expr2);
}
