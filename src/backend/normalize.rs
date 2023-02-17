use super::*;
use crate::frontend::ast::*;
use std::collections::{HashMap, HashSet};

#[allow(dead_code)]
pub struct DataCons {
    cons: Ident,
    pars: Vec<Type>,
    // belongs to which DataDecl
    data: Ident,
}

#[allow(dead_code)]
pub struct DataDecl {
    name: Ident,
    pars: Vec<Ident>,
    // reference to DataCons
    cons: Vec<Ident>,
}

#[allow(dead_code)]
pub struct TypeDecl {
    name: Ident,
    pars: Vec<Ident>,
    typ: Type,
}

pub struct Normalize {
    cons_env: HashMap<Ident, DataCons>,
    data_env: HashMap<Ident, DataDecl>,
    type_env: HashMap<Ident, TypeDecl>,
}

impl Normalize {
    pub fn new() -> Normalize {
        Normalize {
            cons_env: HashMap::new(),
            data_env: HashMap::new(),
            type_env: HashMap::new(),
        }
    }
    pub fn run(expr: &Expr) -> MExpr {
        let mut pass = Normalize::new();
        pass.normalize_top(expr)
    }

    fn get_cons_index(&self, cons: &Ident) -> usize {
        let data = self.cons_env[cons].data;
        self.data_env[&data]
            .cons
            .iter()
            .position(|cons2| *cons == *cons2)
            .unwrap()
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
    fn normalize(&mut self, expr: &Expr, hole: Ident, ctx: MExpr) -> MExpr {
        match expr {
            Expr::Lit { lit, .. } => subst(ctx, hole, (*lit).into()),
            Expr::Var { var, .. } => subst(ctx, hole, Atom::Var(*var)),
            Expr::Prim { prim, args, .. } => {
                // normalize(@iadd(e1,e2), hole, ctx) =
                // normalize(e2,x2,normalize(e1,x1, let hole = iadd(x1,x2) in ctx))
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
                            cont: Box::new(ctx),
                        }
                    }
                    OpPrim::Binary(prim) => {
                        assert!(args.len() == 2);
                        MExpr::BinOp {
                            bind: hole,
                            prim,
                            arg1: Atom::Var(tempvars[0]),
                            arg2: Atom::Var(tempvars[1]),
                            cont: Box::new(ctx),
                        }
                    }
                };

                tempvars
                    .into_iter()
                    .zip(args.iter())
                    .fold(stmt, |res, (bind, arg)| self.normalize(arg, bind, res))
            }
            Expr::Fun { pars, body, .. } => {
                // normalize(fun(x,y) => e, hole, ctx) =
                // let f(x,y) = normalize_top(e) in ctx[hole:=f]
                let funcvar = Ident::generate('f');
                MExpr::LetIn {
                    decls: vec![MDecl {
                        func: funcvar,
                        pars: pars.clone(),
                        body: self.normalize_top(body),
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
                let funcvar = Ident::generate('f');
                let argvars: Vec<Ident> = args.iter().map(|_| Ident::generate('x')).collect();
                let res = MExpr::Call {
                    bind: hole,
                    func: Atom::Var(funcvar),
                    args: argvars.iter().map(|arg| Atom::Var(*arg)).collect(),
                    cont: Box::new(ctx),
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
                // normalize(f(e1,..,en), hole, ctx) =
                // normalize(en,xn,
                //   ...
                //     normalize(e1,x1,
                //       let hole = f(x1,...,xn) in ctx))...)
                let argvars: Vec<Ident> = args.iter().map(|_| Ident::generate('x')).collect();
                let res = MExpr::ExtCall {
                    bind: hole,
                    func: *func,
                    args: argvars.iter().map(|arg| Atom::Var(*arg)).collect(),
                    cont: Box::new(ctx),
                };
                let res = argvars
                    .iter()
                    .cloned()
                    .zip(args.into_iter())
                    .fold(res, |res, (bind, arg)| self.normalize(arg, bind, res));
                res
            }
            Expr::Cons { cons, args, .. } => {
                // normalize(ci(e1,..,en), hole, ctx) =
                // normalize(en,xn,
                //   ...
                //     normalize(e1,x1,
                //       normalize(e0,f,
                //         let m = alloc(n);
                //         store m[0] = i;
                //         store m[1] = x1;
                //         ......
                //         store m[n] = xn;
                //         let hole = move(m);
                //         ctx )...)
                let m = Ident::generate('m');
                let argvars: Vec<Ident> = args.iter().map(|_| Ident::generate('x')).collect();
                let res = MExpr::UnOp {
                    bind: hole,
                    prim: UnOpPrim::Move,
                    arg1: Atom::Var(m),
                    cont: Box::new(ctx),
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
                    arg2: Atom::Int(self.get_cons_index(cons) as i64),
                    cont: Box::new(res),
                };

                let arity = self.cons_env[cons].pars.len();

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
                        ctx
                    ) =

                    normalize(e1,x1,
                        normalize(e2,x2,
                            ...
                            normalize(en,xn,)
                                normalize(res,hole,ctx)
                    ))...)
                */
                self.normalize_block(block, hole, ctx)
            }
            Expr::Case { expr, rules, .. } => {
                /*
                    normalize(
                        case etop of
                        | pattern_1 => e_0
                        ......
                        | pattern_n => e_n-1
                    hole,
                    ctx
                    ) =

                    normalize(etop, o
                        block
                            a_0(..) = e_0;
                            ......
                            a_n-1(..) = e_n;
                        in
                            compile_match(
                                (o),
                                (pattern_1)
                                ......
                                (pattern_n),
                                (a_1(..),......,a_n-1(..))
                            ).chain(hole,ctx)
                        end
                    )
                */
                let etop = Ident::generate('o');

                let mut decls: Vec<MDecl> = Vec::new();

                let (matrix, acts): (Vec<Vec<_>>, Vec<_>) = rules
                    .into_iter()
                    .map(|rule| {
                        let Rule { patn, body, .. } = rule;
                        let func = Ident::generate('a');
                        let pars = patn.get_freevars();
                        let args = pars.iter().map(|var| Atom::Var(*var)).collect();

                        let decl = MDecl {
                            func,
                            pars,
                            body: self.normalize_top(body),
                        };
                        decls.push(decl);

                        let act = MExpr::make_tail_call(func, args);

                        (vec![patn.clone()], act)
                    })
                    .unzip();

                let objs = vec![etop];

                let mat = PatnMatrix { objs, matrix, acts };
                let cont = Box::new(self.compile_match(&mat, hole, ctx));
                self.normalize(expr, etop, MExpr::LetIn { decls, cont })
            }
            Expr::Ifte {
                cond, trbr, flbr, ..
            } => {
                /*
                    normalize(
                        if cond then trbr else flbr,
                        hole,
                        ctx
                    ) =
                    normalize(cond, x,
                        let hole = if x
                            then normalize_top(trbr)
                            else normalize_top(flbr);
                        ctx
                    )
                */
                let x = Ident::generate('x');
                let cont = MExpr::Ifte {
                    bind: hole,
                    arg1: Atom::Var(x),
                    brch1: Box::new(self.normalize_top(trbr)),
                    brch2: Box::new(self.normalize_top(flbr)),
                    cont: Box::new(ctx),
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
                        Decl::Data {
                            name, pars, vars, ..
                        } => {
                            for var in vars {
                                let Varient { cons, pars, .. } = var;
                                self.cons_env.insert(
                                    *cons,
                                    DataCons {
                                        cons: *cons,
                                        pars: pars.clone(),
                                        data: *name,
                                    },
                                );
                            }
                            self.data_env.insert(
                                *name,
                                DataDecl {
                                    name: *name,
                                    pars: pars.clone(),
                                    cons: vars.iter().map(|var| var.cons).collect(),
                                },
                            );
                            None
                        }
                        Decl::Type {
                            name, pars, typ, ..
                        } => {
                            self.type_env.insert(
                                *name,
                                TypeDecl {
                                    name: *name,
                                    pars: pars.clone(),
                                    typ: typ.clone(),
                                },
                            );
                            None
                        }
                        Decl::Extern { .. } => None,
                    })
                    .collect();
                let cont = Box::new(self.normalize_block(block, hole, ctx));
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
                let bind = Ident::generate('w');
                self.normalize(expr, bind, cont)
            }
        })
    }

    fn compile_match_top(&mut self, mat: &PatnMatrix) -> MExpr {
        let r = Ident::generate('r');
        self.compile_match(mat, r, MExpr::Retn { arg1: Atom::Var(r) })
    }

    fn compile_match(&mut self, mat: &PatnMatrix, hole: Ident, ctx: MExpr) -> MExpr {
        if mat.is_empty() {
            panic!("pattern match not exhaustive!")
        } else if mat.first_row_aways_match() {
            let cont = mat.acts[0].clone();
            mat.matrix[0]
                .iter()
                .zip(mat.objs.iter())
                .flat_map(|(patn, obj)| match patn {
                    Pattern::Var { var, .. } => Some((var, obj)),
                    Pattern::Lit { .. } => unreachable!(),
                    Pattern::Cons { .. } => unreachable!(),
                    Pattern::Wild { .. } => None,
                })
                .fold(cont, |cont, (var, obj)| MExpr::UnOp {
                    bind: *var,
                    prim: UnOpPrim::Move,
                    arg1: Atom::Var(*obj),
                    cont: Box::new(cont),
                })
        } else {
            let j = self.get_best_col();
            match self.get_col_type(mat, j) {
                ColType::Any => self.match_default(mat, j),
                ColType::Data(data) => {
                    let cons_set = mat.get_cons_set(j);
                    let mut exhaustive = true;
                    let brchs = self.data_env[&data]
                        .cons
                        .clone()
                        .into_iter()
                        .enumerate()
                        .into_iter()
                        .flat_map(|(i, cons)| {
                            if cons_set.contains(&cons) {
                                let arity = self.cons_env[&cons].pars.len();
                                Some((i, self.match_specialize(mat, j, cons, arity)))
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
                            cont: Box::new(ctx),
                        }),
                    }
                }
                ColType::Lit(LitType::Int) => {
                    todo!()
                }
                ColType::Lit(LitType::Real) => {
                    panic!("pattern match on real numbers are not allowed!");
                }
                ColType::Lit(LitType::Bool) => {
                    todo!()
                }
                ColType::Lit(LitType::Char) => {
                    todo!()
                }
                ColType::Lit(LitType::Unit) => {
                    todo!()
                }
            }
        }
    }

    fn get_best_col(&self) -> usize {
        // let mut counts = Vec::with_capacity(self.get_col_num());
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
                    ColType::Any => res = ColType::Data(self.cons_env[&cons].data),
                    ColType::Lit(_) => {
                        panic!("pattern match not well-typed!")
                    }
                    ColType::Data(data) => {
                        if data != self.cons_env[&cons].data {
                            panic!("pattern match not well-typed!")
                        }
                    }
                },
                Pattern::Wild { .. } => {}
            }
        }
        res
    }

    fn match_specialize(&mut self, mat: &PatnMatrix, j: usize, cons: Ident, arity: usize) -> MExpr {
        let matchee = mat.objs[j];
        let new_objs: Vec<Ident> = (0..arity).map(|_| Ident::generate('o')).collect();
        let mut bindings: Vec<(Ident, Ident)> = Vec::new();

        let (matrix, acts): (Vec<Vec<_>>, _) = mat
            .matrix
            .iter()
            .zip(mat.acts.iter())
            .flat_map(|(row, act)| match &row[j] {
                Pattern::Lit { .. } => {
                    unreachable!()
                }
                Pattern::Var { var, span } => {
                    let new = row[..j]
                        .iter()
                        .chain(std::iter::repeat(&Pattern::Wild { span: *span }).take(arity))
                        .chain(row[j + 1..].iter())
                        .cloned()
                        .collect();
                    bindings.push((*var, matchee));
                    Some((new, act.clone()))
                }
                Pattern::Cons {
                    cons: cons2, pars, ..
                } if *cons2 == cons => {
                    assert_eq!(pars.len(), arity);
                    let new = row[..j]
                        .iter()
                        .chain(pars.iter())
                        .chain(row[j + 1..].iter())
                        .cloned()
                        .collect();
                    Some((new, act.clone()))
                }
                Pattern::Cons { .. } => None,
                Pattern::Wild { span } => {
                    let new = row[..j]
                        .iter()
                        .chain(std::iter::repeat(&Pattern::Wild { span: *span }).take(arity))
                        .chain(row[j + 1..].iter())
                        .cloned()
                        .collect();
                    Some((new, act.clone()))
                }
            })
            .unzip();

        let objs = mat.objs[..j]
            .iter()
            .chain(new_objs.iter())
            .chain(mat.objs[j + 1..].iter())
            .cloned()
            .collect();

        let new_mat = PatnMatrix { objs, matrix, acts };
        let cont = self.compile_match_top(&new_mat);

        let cont = bindings
            .into_iter()
            .fold(cont, |cont, (var, obj)| MExpr::UnOp {
                bind: var,
                prim: UnOpPrim::Move,
                arg1: Atom::Var(obj),
                cont: Box::new(cont),
            });

        let cont = new_objs
            .into_iter()
            .enumerate()
            .fold(cont, |cont, (i, obj)| MExpr::Load {
                bind: obj,
                arg1: Atom::Var(matchee),
                index: i + 1,
                cont: Box::new(cont),
            });

        cont
    }

    fn match_default(&mut self, mat: &PatnMatrix, j: usize) -> MExpr {
        let matchee = mat.objs[j];
        let mut bindings: Vec<(Ident, Ident)> = Vec::new();

        let (matrix, acts): (Vec<Vec<_>>, _) = mat
            .matrix
            .iter()
            .zip(mat.acts.iter())
            .flat_map(|(row, act)| match &row[j] {
                Pattern::Lit { .. } => None,
                Pattern::Var { var, .. } => {
                    let new = row[..j]
                        .iter()
                        .chain(row[j + 1..].iter())
                        .cloned()
                        .collect();
                    bindings.push((*var, matchee));
                    Some((new, act.clone()))
                }
                Pattern::Cons { .. } => None,
                Pattern::Wild { .. } => {
                    let new = row[..j]
                        .iter()
                        .chain(row[j + 1..].iter())
                        .cloned()
                        .collect();
                    Some((new, act.clone()))
                }
            })
            .unzip();

        let objs = mat.objs[..j]
            .iter()
            .chain(mat.objs[j + 1..].iter())
            .cloned()
            .collect();

        let new_mat = PatnMatrix { objs, matrix, acts };
        let cont = self.compile_match_top(&new_mat);

        let cont = bindings
            .into_iter()
            .fold(cont, |cont, (var, obj)| MExpr::UnOp {
                bind: var,
                prim: UnOpPrim::Move,
                arg1: Atom::Var(obj),
                cont: Box::new(cont),
            });

        cont
    }
}

fn subst(expr: MExpr, hole: Ident, atom: Atom) -> MExpr {
    // subst(expr,hole,atom) ~=~ let hole = move(atom); expr
    // it will be substituted in constant-fold pass anyway
    MExpr::UnOp {
        bind: hole,
        prim: UnOpPrim::Move,
        arg1: atom,
        cont: Box::new(expr),
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ColType {
    Any,
    Data(Ident),
    Lit(LitType),
}

// assume that more than one row and more than one column
#[derive(Debug, Clone)]
pub struct PatnMatrix {
    objs: Vec<Ident>,
    matrix: Vec<Vec<Pattern>>,
    acts: Vec<MExpr>,
}

impl PatnMatrix {
    #[allow(dead_code)]
    fn get_row_num(&self) -> usize {
        self.matrix.len()
    }
    #[allow(dead_code)]
    fn get_col_num(&self) -> usize {
        if self.matrix.is_empty() {
            return 0;
        }
        let res = self.matrix[0].len();
        for row in &self.matrix[1..] {
            assert_eq!(row.len(), res);
        }
        res
    }
    fn is_empty(&self) -> bool {
        self.matrix.is_empty()
    }
    fn first_row_aways_match(&self) -> bool {
        assert!(!self.matrix.is_empty());
        self.matrix[0].iter().all(|p| p.is_wild_or_var())
    }

    fn get_cons_set(&self, j: usize) -> HashSet<Ident> {
        let mut set = HashSet::new();
        for row in self.matrix.iter() {
            match &row[j] {
                Pattern::Lit { .. } => {}
                Pattern::Var { .. } => {}
                Pattern::Cons { cons, .. } => {
                    set.insert(*cons);
                }
                Pattern::Wild { .. } => {}
            }
        }
        set
    }
}

#[test]
fn normalize_test() {
    use super::anf_build::*;
    use crate::frontend::parser::*;
    use crate::frontend::renamer::Renamer;

    let string = r#"
@iadd(@iadd(1,2),@iadd(3,4))
    "#;
    let mut par = Parser::new(string);
    let mut expr1 = parse_expr(&mut par).unwrap();
    Renamer::run(&mut expr1).unwrap();
    let expr1 = Normalize::run(&expr1);
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
        let f = fun(x) => @iadd(x,1);
        f(42)
    end
    "#;
    let mut par = Parser::new(string);
    let mut expr1 = parse_expr(&mut par).unwrap();
    Renamer::run(&mut expr1).unwrap();
    let expr1 = Normalize::run(&expr1);
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
    use crate::frontend::parser::*;
    use crate::frontend::renamer::Renamer;
    let string = r#"
letrec
    data List[T] =
    | Cons(T,List[T])
    | Nil
    end
    fun length[T](lst: List[T]): Int =
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
    // println!("{expr1:#?}");
    let _expr1 = Normalize::run(&expr1);
    // println!("{expr1}");
}
