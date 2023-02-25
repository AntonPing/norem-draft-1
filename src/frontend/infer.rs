use itertools::Itertools;
use std::{
    collections::{HashMap, HashSet},
    fmt,
};

use super::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeBase {
    Lit(LitType),
    Gen(Ident),
    Var(Ident),
    Fun(Vec<TypeBase>, Box<TypeBase>),
    App(Ident, Vec<TypeBase>),
}

impl fmt::Display for TypeBase {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeBase::Lit(lit) => write!(f, "{lit}"),
            TypeBase::Gen(var) => write!(f, "{}", var.name),
            TypeBase::Var(var) => write!(f, "{var}"),
            TypeBase::Fun(pars, res) => {
                let pars = pars.iter().format(&", ");
                write!(f, "fn({pars}) -> {res}")
            }
            TypeBase::App(cons, args) => {
                let args = args.iter().format(&", ");
                write!(f, "{cons}({args})")
            }
        }
    }
}

impl TypeBase {
    fn new_type() -> TypeBase {
        TypeBase::Var(Ident::generate('t'))
    }
    fn rename_type(ident: &Ident) -> TypeBase {
        TypeBase::Var(ident.uniquify())
    }
    fn subst(&self, map: &HashMap<Ident, TypeBase>) -> TypeBase {
        match self {
            TypeBase::Lit(lit) => TypeBase::Lit(*lit),
            TypeBase::Gen(gen) => {
                if let Some(typ) = map.get(&gen) {
                    typ.clone()
                } else {
                    TypeBase::Gen(*gen)
                }
            }
            TypeBase::Var(var) => {
                if let Some(typ) = map.get(&var) {
                    typ.clone()
                } else {
                    TypeBase::Var(*var)
                }
            }
            TypeBase::Fun(pars, res) => {
                let pars = pars.iter().map(|par| par.subst(map)).collect();
                let res = Box::new(res.subst(map));
                TypeBase::Fun(pars, res)
            }
            TypeBase::App(cons, args) => {
                let args = args.iter().map(|arg| arg.subst(map)).collect();
                TypeBase::App(*cons, args)
            }
        }
    }
    fn is_free(&self, var: &Ident) -> bool {
        match self {
            TypeBase::Lit(_) | TypeBase::Gen(_) => false,
            TypeBase::Var(var2) => *var == *var2,
            TypeBase::Fun(pars, res) => pars.iter().any(|par| par.is_free(var)) || res.is_free(var),
            TypeBase::App(cons, args) => (*cons == *var) || args.iter().any(|arg| arg.is_free(var)),
        }
    }
}

impl Builtin {
    fn get_pars_type(&self) -> Vec<TypeBase> {
        use Builtin::*;
        match self {
            IAdd | ISub | IMul | IDiv | IRem | ICmpGr | ICmpLs | ICmpEq | ICmpLe | ICmpGe
            | ICmpNe => {
                vec![TypeBase::Lit(LitType::Int), TypeBase::Lit(LitType::Int)]
            }
            INeg => {
                vec![TypeBase::Lit(LitType::Int)]
            }
            RAdd | RSub | RMul | RDiv | RCmpGr | RCmpLs | RCmpEq | RCmpLe | RCmpGe | RCmpNe => {
                vec![TypeBase::Lit(LitType::Real), TypeBase::Lit(LitType::Real)]
            }
            BAnd | BOr => {
                vec![TypeBase::Lit(LitType::Bool), TypeBase::Lit(LitType::Bool)]
            }
            BNot => {
                vec![TypeBase::Lit(LitType::Bool)]
            }
        }
    }
    fn get_res_type(&self) -> TypeBase {
        use Builtin::*;
        match self {
            IAdd | ISub | IMul | IDiv | IRem | INeg => TypeBase::Lit(LitType::Int),
            RAdd | RSub | RMul | RDiv => TypeBase::Lit(LitType::Real),
            BAnd | BOr | BNot | ICmpGr | ICmpLs | ICmpEq | ICmpLe | ICmpGe | ICmpNe | RCmpGr
            | RCmpLs | RCmpEq | RCmpLe | RCmpGe | RCmpNe => TypeBase::Lit(LitType::Bool),
        }
    }
}

#[derive(Clone, Debug)]
pub enum UnifyError {
    CantUnify(TypeBase, TypeBase),
    CantUnifyVec(Vec<TypeBase>, Vec<TypeBase>),
    OccurCheckFailed(Ident, TypeBase),
    DiffConstr(Ident, Ident),
}

#[derive(Copy, Clone, Debug)]
pub enum InferErrorTitle {
    PrimAppError,
    FuncAppError,
    ExtCallError,
    ConstructorError,
    IfteCondError,
    IfteBrchError,
    CaseBrchError,
    LetAnnoError,
    PatternError,
    FuncDeclError,
}
#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct InferError {
    pub title: InferErrorTitle,
    pub spans: Vec<(&'static str, Span)>,
    pub errs: Vec<UnifyError>,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
struct FuncDecl {
    name: Ident,
    gens: Vec<Ident>,
    pars: Vec<(Ident, TypeBase)>,
    res: TypeBase,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
struct DataCons {
    cons: Ident,
    pars: Vec<TypeBase>,
    // belongs to which DataDecl
    data: Ident,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
struct DataDecl {
    name: Ident,
    gens: Vec<Ident>,
    vars: Vec<Ident>,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
struct TypeDecl {
    name: Ident,
    gens: Vec<Ident>,
    typ: TypeBase,
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
struct ExternDecl {
    name: InternStr,
    gens: Vec<Ident>,
    typ: TypeBase,
}

pub struct Infer {
    val_env: HashMap<Ident, TypeBase>,
    func_env: HashMap<Ident, FuncDecl>,
    cons_env: HashMap<Ident, DataCons>,
    data_env: HashMap<Ident, DataDecl>,
    type_env: HashMap<Ident, TypeDecl>,
    extern_env: HashMap<InternStr, ExternDecl>,
    generic_set: HashSet<Ident>,
    unify_subst: HashMap<Ident, TypeBase>,
    error: Vec<InferError>,
}

impl Infer {
    pub fn new() -> Infer {
        Infer {
            val_env: HashMap::new(),
            func_env: HashMap::new(),
            cons_env: HashMap::new(),
            data_env: HashMap::new(),
            type_env: HashMap::new(),
            extern_env: HashMap::new(),
            generic_set: HashSet::new(),
            unify_subst: HashMap::new(),
            error: Vec::new(),
        }
    }

    pub fn run(expr: &Expr) -> Result<(), Vec<InferError>> {
        let mut pass = Infer::new();
        pass.infer_expr(expr);
        if pass.error.is_empty() {
            Ok(())
        } else {
            pass.merge_errors();
            Err(pass.error)
        }
    }

    fn merge(&self, ty: &TypeBase) -> TypeBase {
        match ty {
            TypeBase::Lit(lit) => TypeBase::Lit(*lit),
            TypeBase::Gen(gen) => TypeBase::Gen(*gen),
            TypeBase::Var(var) => {
                if let Some(res) = self.unify_subst.get(var).cloned() {
                    self.merge(&res)
                } else {
                    TypeBase::Var(*var)
                }
            }
            TypeBase::Fun(pars, res) => {
                let pars = pars.iter().map(|par| self.merge(par)).collect();
                let res = self.merge(res);
                TypeBase::Fun(pars, Box::new(res))
            }
            TypeBase::App(cons, args) => {
                let args = args.iter().map(|arg| self.merge(arg)).collect();
                TypeBase::App(*cons, args)
            }
        }
    }

    fn unify(&mut self, lhs: &TypeBase, rhs: &TypeBase) -> Vec<UnifyError> {
        let mut errs = Vec::new();
        self.unify_aux(&mut errs, lhs, rhs);
        errs
    }

    fn unify_vec(&mut self, lhs: &Vec<TypeBase>, rhs: &Vec<TypeBase>) -> Vec<UnifyError> {
        let mut errs = Vec::new();
        self.unify_vec_aux(&mut errs, lhs, rhs);
        errs
    }

    fn assign(&mut self, errs: &mut Vec<UnifyError>, x: &Ident, ty: &TypeBase) {
        assert!(!self.generic_set.contains(x));
        if let Some(res) = self.unify_subst.get(&x).cloned() {
            self.unify_aux(errs, ty, &res)
        } else {
            let ty = self.merge(ty);
            self.unify_subst.insert(*x, ty);
        }
    }

    fn unify_aux(&mut self, errs: &mut Vec<UnifyError>, lhs: &TypeBase, rhs: &TypeBase) {
        let start_len = errs.len();
        match (lhs, rhs) {
            (TypeBase::Lit(a), TypeBase::Lit(b)) if a == b => {
                // do nothing
            }
            (TypeBase::Gen(x), TypeBase::Gen(y)) if *x == *y => {
                // do nothing
            }
            (TypeBase::Var(x), TypeBase::Var(y)) if *x == *y => {
                // do nothing
            }
            (TypeBase::Var(x), ty) | (ty, TypeBase::Var(x)) => {
                if ty.is_free(&x) {
                    errs.push(UnifyError::OccurCheckFailed(*x, ty.clone()))
                } else {
                    self.assign(errs, x, ty);
                }
            }
            (TypeBase::Fun(pars1, res1), TypeBase::Fun(pars2, res2)) => {
                self.unify_vec_aux(errs, pars1, pars2);
                self.unify_aux(errs, res1, res2);
                if errs.len() > start_len {
                    errs.push(UnifyError::CantUnify(lhs.clone(), rhs.clone()))
                }
            }
            (TypeBase::App(cons1, args1), TypeBase::App(cons2, args2)) => {
                // todo: type name alias
                if cons1 != cons2 {
                    errs.push(UnifyError::DiffConstr(*cons1, *cons2))
                }
                self.unify_vec_aux(errs, args1, args2);
                if errs.len() > start_len {
                    errs.push(UnifyError::CantUnify(lhs.clone(), rhs.clone()))
                }
            }
            (lhs, rhs) => errs.push(UnifyError::CantUnify(lhs.clone(), rhs.clone())),
        }
    }

    fn unify_vec_aux(
        &mut self,
        errs: &mut Vec<UnifyError>,
        lhs: &Vec<TypeBase>,
        rhs: &Vec<TypeBase>,
    ) {
        if lhs.len() != rhs.len() {
            errs.push(UnifyError::CantUnifyVec(lhs.clone(), rhs.clone()))
        } else {
            let start_len = errs.len();
            for (lhs, rhs) in lhs.into_iter().zip(rhs.into_iter()) {
                self.unify_aux(errs, lhs, rhs)
            }
            if errs.len() > start_len {
                errs.push(UnifyError::CantUnifyVec(lhs.clone(), rhs.clone()))
            }
        }
    }

    fn instantiate_func(&self, func: &Ident) -> TypeBase {
        let decl = self.func_env[func].clone();
        if decl.gens.is_empty() {
            let pars = decl.pars.iter().map(|(_, typ)| typ.clone()).collect();
            let res = decl.res.clone();
            TypeBase::Fun(pars, Box::new(res))
        } else {
            let map: HashMap<_, _> = decl
                .gens
                .iter()
                .map(|x| (*x, TypeBase::rename_type(x)))
                .collect();
            let pars = decl.pars.iter().map(|(_, typ)| typ.subst(&map)).collect();
            let res = decl.res.subst(&map);
            TypeBase::Fun(pars, Box::new(res))
        }
    }

    fn instantiate_cons(&self, cons: &Ident) -> (Vec<TypeBase>, TypeBase) {
        let cons_decl = self.cons_env[cons].clone();
        let data_decl = self.data_env[&cons_decl.data].clone();
        if data_decl.gens.is_empty() {
            let pars = cons_decl.pars.clone();
            let res = TypeBase::App(cons_decl.data, Vec::new());
            (pars, res)
        } else {
            let map: HashMap<_, _> = data_decl
                .gens
                .iter()
                .map(|x| (*x, TypeBase::rename_type(x)))
                .collect();
            let pars = cons_decl.pars.iter().map(|typ| typ.subst(&map)).collect();
            let gens = data_decl.gens.iter().map(|x| map[&x].clone()).collect();
            let res = TypeBase::App(cons_decl.data, gens);
            (pars, res)
        }
    }

    fn instantiate_extcall(&self, name: &InternStr) -> TypeBase {
        let decl = self.extern_env[name].clone();
        if decl.gens.is_empty() {
            decl.typ.clone()
        } else {
            let map: HashMap<_, _> = decl
                .gens
                .iter()
                .map(|x| (*x, TypeBase::rename_type(x)))
                .collect();
            decl.typ.subst(&map)
        }
    }

    fn infer_expr(&mut self, expr: &Expr) -> TypeBase {
        match expr {
            Expr::Lit { lit, .. } => TypeBase::Lit(lit.get_lit_type()),
            Expr::Var { var, .. } => {
                if self.func_env.get(var).is_some() {
                    self.instantiate_func(var)
                } else if let Some(typ) = self.val_env.get(var) {
                    typ.clone()
                } else {
                    panic!("unbounded variable!");
                }
            }
            Expr::Prim { prim, args, span } => {
                let args_ty = args
                    .iter()
                    .map(|arg| self.infer_expr(arg))
                    .collect::<Vec<_>>();
                let pars_ty = prim.get_pars_type();
                let errs = self.unify_vec(&args_ty, &pars_ty);
                if !errs.is_empty() {
                    self.error.push(InferError {
                        title: InferErrorTitle::PrimAppError,
                        spans: vec![("here is the application:", span.clone())],
                        errs,
                    });
                }
                prim.get_res_type()
            }
            Expr::Fun { pars, body, .. } => {
                let pars = pars
                    .iter()
                    .map(|par| {
                        let typ = TypeBase::new_type();
                        self.val_env.insert(*par, typ.clone());
                        typ
                    })
                    .collect();
                let res = self.infer_expr(body);
                TypeBase::Fun(pars, Box::new(res))
            }
            Expr::App { func, args, span } => {
                let func_ty = self.infer_expr(func);
                let args_ty = args
                    .iter()
                    .map(|arg| self.infer_expr(arg))
                    .collect::<Vec<_>>();
                let res_ty = TypeBase::new_type();
                let func_ty2 = TypeBase::Fun(args_ty.clone(), Box::new(res_ty.clone()));
                let errs = self.unify(&func_ty, &func_ty2);
                if !errs.is_empty() {
                    self.error.push(InferError {
                        title: InferErrorTitle::FuncAppError,
                        spans: vec![("here is the application:", span.clone())],
                        errs,
                    });
                }
                res_ty
            }
            Expr::ExtCall { func, args, span } => {
                let func_ty = self.instantiate_extcall(func);
                let args_ty = args
                    .iter()
                    .map(|arg| self.infer_expr(arg))
                    .collect::<Vec<_>>();
                let res_ty = TypeBase::new_type();
                let func_ty2 = TypeBase::Fun(args_ty.clone(), Box::new(res_ty.clone()));
                let errs = self.unify(&func_ty, &func_ty2);
                if !errs.is_empty() {
                    self.error.push(InferError {
                        title: InferErrorTitle::ExtCallError,
                        spans: vec![("here is the application:", span.clone())],
                        errs,
                    });
                }
                res_ty
            }
            Expr::Cons { cons, args, span } => {
                let (pars_ty, res_ty) = self.instantiate_cons(cons);
                let args_ty = args
                    .iter()
                    .map(|arg| self.infer_expr(arg))
                    .collect::<Vec<_>>();
                let errs = self.unify_vec(&args_ty, &pars_ty);
                if !errs.is_empty() {
                    self.error.push(InferError {
                        title: InferErrorTitle::ConstructorError,
                        spans: vec![("here is the source code:", span.clone())],
                        errs,
                    });
                }
                res_ty
            }
            Expr::Ifte {
                cond, trbr, flbr, ..
            } => {
                let cond_ty = self.infer_expr(cond);
                let errs = self.unify(&cond_ty, &TypeBase::Lit(LitType::Bool));
                if !errs.is_empty() {
                    self.error.push(InferError {
                        title: InferErrorTitle::IfteCondError,
                        spans: vec![("here is the source code:", cond.span().clone())],
                        errs,
                    });
                }
                let trbr_ty = self.infer_expr(trbr);
                let flbr_ty = self.infer_expr(flbr);
                let errs = self.unify(&trbr_ty, &flbr_ty);
                if !errs.is_empty() {
                    self.error.push(InferError {
                        title: InferErrorTitle::IfteBrchError,
                        spans: vec![
                            ("here is the true-branch:", trbr.span().clone()),
                            ("here is the false-branch:", flbr.span().clone()),
                        ],
                        errs,
                    });
                }
                trbr_ty
            }
            Expr::Begin { block, .. } => self.infer_block(block),
            Expr::Case { expr, rules, .. } => {
                let expr_ty = self.infer_expr(expr);
                let res_ty = TypeBase::new_type();
                for rule in rules {
                    let Rule { patn, body, .. } = rule;
                    self.infer_pattern(patn, &expr_ty);
                    let body_ty = self.infer_expr(body);
                    let errs = self.unify(&body_ty, &res_ty);
                    if !errs.is_empty() {
                        self.error.push(InferError {
                            title: InferErrorTitle::CaseBrchError,
                            spans: vec![("here is the branch body:", body.span().clone())],
                            errs,
                        });
                    }
                }
                res_ty
            }
            Expr::Letrec { decls, block, .. } => {
                for decl in decls {
                    self.infer_decl(decl);
                }
                self.infer_block(block)
            }
        }
    }

    fn infer_block(&mut self, block: &Block) -> TypeBase {
        let Block { stmts, retn, .. } = block;
        for stmt in stmts {
            match stmt {
                Stmt::Bind {
                    bind, typ, expr, ..
                } => {
                    let expr_ty = self.infer_expr(expr);
                    if let Some(typ) = typ {
                        let typ_ty = self.read_type(typ);
                        let errs = self.unify(&expr_ty, &typ_ty);
                        if !errs.is_empty() {
                            self.error.push(InferError {
                                title: InferErrorTitle::LetAnnoError,
                                spans: vec![
                                    ("here is the expression:", expr.span().clone()),
                                    ("here is the type annotation:", typ.span().clone()),
                                ],
                                errs,
                            });
                        }
                    }
                    self.val_env.insert(*bind, expr_ty);
                }
                Stmt::Do { expr, .. } => {
                    let _expr_ty = self.infer_expr(expr);
                }
            }
        }
        if let Some(retn) = retn {
            self.infer_expr(retn)
        } else {
            TypeBase::Lit(LitType::Unit)
        }
    }

    fn infer_pattern(&mut self, patn: &Pattern, matchee: &TypeBase) {
        match patn {
            Pattern::Var { var, .. } => {
                self.val_env.insert(*var, matchee.clone());
            }
            Pattern::Lit { lit, span } => {
                let errs = self.unify(matchee, &TypeBase::Lit(lit.get_lit_type()));
                if !errs.is_empty() {
                    self.error.push(InferError {
                        title: InferErrorTitle::PatternError,
                        spans: vec![("here is the pattern:", span.clone())],
                        errs,
                    });
                }
            }
            Pattern::Cons { cons, pars, span } => {
                let (pars_ty, res_ty) = self.instantiate_cons(cons);
                if pars.len() != pars_ty.len() {
                    self.error.push(InferError {
                        title: InferErrorTitle::PatternError,
                        spans: vec![("here is the pattern:", span.clone())],
                        errs: vec![],
                    });
                } else {
                    pars.iter().zip(pars_ty.iter()).for_each(|(par, ty)| {
                        self.infer_pattern(par, &ty);
                    });
                }
                let errs = self.unify(&matchee, &res_ty);
                if !errs.is_empty() {
                    self.error.push(InferError {
                        title: InferErrorTitle::PatternError,
                        spans: vec![("here is the pattern:", span.clone())],
                        errs,
                    });
                }
            }
            Pattern::Wild { .. } => {}
        }
    }

    fn infer_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Func {
                name,
                gens,
                pars,
                res,
                body,
                span,
            } => {
                self.generic_set.extend(gens.iter());
                let pars: Vec<(Ident, TypeBase)> = pars
                    .iter()
                    .map(|(par, typ)| {
                        let typ = self.read_type(typ);
                        (*par, typ)
                    })
                    .collect();
                let res_ty = self.read_type(res);
                let decl = FuncDecl {
                    name: *name,
                    gens: gens.clone(),
                    pars: pars.clone(),
                    res: res_ty.clone(),
                };
                self.func_env.insert(*name, decl);
                for (par, typ) in pars {
                    self.val_env.insert(par, typ);
                }
                let body_ty = self.infer_expr(body);
                let errs = self.unify(&body_ty, &res_ty);
                if !errs.is_empty() {
                    self.error.push(InferError {
                        title: InferErrorTitle::FuncDeclError,
                        spans: vec![("here is the declaration:", span.clone())],
                        errs,
                    });
                }
            }
            Decl::Data {
                name, pars, vars, ..
            } => {
                self.generic_set.extend(pars.iter());
                let cons = vars
                    .iter()
                    .map(|var| {
                        let Varient { cons, pars, .. } = var;
                        let pars = pars.iter().map(|par| self.read_type(par)).collect();
                        let decl = DataCons {
                            cons: *cons,
                            pars,
                            data: *name,
                        };
                        self.cons_env.insert(*cons, decl);
                        *cons
                    })
                    .collect();
                let decl = DataDecl {
                    name: *name,
                    gens: pars.clone(),
                    vars: cons,
                };
                self.data_env.insert(*name, decl);
            }
            Decl::Type {
                name, pars, typ, ..
            } => {
                self.generic_set.extend(pars.iter());
                let decl = TypeDecl {
                    name: *name,
                    gens: pars.clone(),
                    typ: self.read_type(typ),
                };
                self.type_env.insert(*name, decl);
            }
            Decl::Extern {
                name, gens, typ, ..
            } => {
                self.generic_set.extend(gens.iter());
                let decl = ExternDecl {
                    name: *name,
                    gens: gens.clone(),
                    typ: self.read_type(typ),
                };
                self.extern_env.insert(*name, decl);
            }
        }
    }

    fn read_type(&mut self, typ: &Type) -> TypeBase {
        match typ {
            Type::Lit { lit, .. } => TypeBase::Lit(*lit),
            Type::Var { var, .. } => {
                if self.generic_set.contains(var) {
                    TypeBase::Gen(*var)
                } else {
                    TypeBase::Var(*var)
                }
            }
            Type::Fun { pars, res, .. } => {
                let pars = pars.iter().map(|par| self.read_type(par)).collect();
                let res = self.read_type(res);
                TypeBase::Fun(pars, Box::new(res))
            }
            Type::App { cons, args, .. } => {
                let args = args.iter().map(|arg| self.read_type(arg)).collect();
                TypeBase::App(*cons, args)
            }
        }
    }

    fn merge_errors(&mut self) {
        let mut vec: Vec<_> = self.error.drain(..).collect();
        for infer_err in &mut vec {
            for unify_err in &mut infer_err.errs {
                *unify_err = self.merge_unify_error(&unify_err);
            }
        }
        self.error = vec;
    }

    fn merge_unify_error(&mut self, err: &UnifyError) -> UnifyError {
        match err {
            UnifyError::CantUnify(lhs, rhs) => {
                let lhs = self.merge(lhs);
                let rhs = self.merge(rhs);
                UnifyError::CantUnify(lhs, rhs)
            }
            UnifyError::CantUnifyVec(lhs, rhs) => {
                let lhs = lhs.iter().map(|ty| self.merge(ty)).collect();
                let rhs = rhs.iter().map(|ty| self.merge(ty)).collect();
                UnifyError::CantUnifyVec(lhs, rhs)
            }
            UnifyError::OccurCheckFailed(x, ty) => {
                let old = self.unify_subst.remove(&x);
                let ty = self.merge(ty);
                if let Some(old) = old {
                    self.unify_subst.insert(*x, old);
                }
                UnifyError::OccurCheckFailed(*x, ty)
            }
            UnifyError::DiffConstr(cons1, cons2) => UnifyError::DiffConstr(*cons1, *cons2),
        }
    }
}

#[test]
#[ignore]
fn type_check_test() {
    use super::parser::*;
    use super::renamer::Renamer;
    let string = r#"
letrec
    extern print_int : fn(Int) -> ();
    data List[T] =
    | Cons(T,List[T])
    | Nil
    end
    func length[T](lst: List[T]): Int =
        case lst of
        | Cons(head,tail) =>
            @iadd(length(tail),length(head))
        | Nil => 0
        end
in
    #print_int(length(Cons(1,Cons(2,Nil))));
end
"#;

    let mut par = Parser::new(string);
    let mut expr = parse_expr(&mut par).unwrap();
    // println!("{}", expr);
    Renamer::run(&mut expr).unwrap();
    // println!("{}", expr);
    let mut tych = Infer::new();
    tych.infer_expr(&expr);
    tych.merge_errors();
    assert!(!tych.error.is_empty());
    /*
    for err in &tych.error {
        println!("{err:#?}");
    }
    */
}
