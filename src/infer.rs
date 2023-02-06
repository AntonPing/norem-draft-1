use itertools::{self, Itertools};
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::Infallible;
use std::fmt::Display;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use crate::ast::*;
use crate::intern::*;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeCell {
    Unbound(Ident, usize),
    Link(MonoType),
}

impl TypeCell {
    fn is_bound(&self) -> bool {
        match self {
            TypeCell::Unbound(_, _) => false,
            TypeCell::Link(_) => true,
        }
    }
    fn unwrap_link(&self) -> &MonoType {
        match self {
            TypeCell::Unbound(_, _) => panic!("failed to unwrap link!"),
            TypeCell::Link(link) => link,
        }
    }
    #[allow(dead_code)]
    fn unwrap_link_mut(&mut self) -> &mut MonoType {
        match self {
            TypeCell::Unbound(_, _) => panic!("failed to unwrap link!"),
            TypeCell::Link(link) => link,
        }
    }
    fn unwrap_level(&self) -> usize {
        match self {
            TypeCell::Unbound(_, level) => *level,
            TypeCell::Link(_) => panic!("failed to unwrap link!"),
        }
    }
}
type MonoType = TypeBase<Infallible>;
type PolyType = TypeBase<()>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeBase<P> {
    Lit(LitType),
    // Var can only appears in PolyType
    Var(Ident, P),
    // mutable cell for constant-time unification
    Cell(Rc<RefCell<TypeCell>>),
    Fun(Vec<TypeBase<P>>, Box<TypeBase<P>>),
    App(Ident, Vec<TypeBase<P>>),
}

impl<P> TypeBase<P> {
    fn uniop(lit: LitType) -> Self {
        TypeBase::Fun(vec![TypeBase::Lit(lit)], Box::new(TypeBase::Lit(lit)))
    }
    fn binop(lit: LitType) -> Self {
        TypeBase::Fun(
            vec![TypeBase::Lit(lit), TypeBase::Lit(lit)],
            Box::new(TypeBase::Lit(lit)),
        )
    }
    fn get_builtin_type(prim: Builtin) -> Self {
        match prim {
            Builtin::IAdd => TypeBase::binop(LitType::Int),
            Builtin::ISub => TypeBase::binop(LitType::Int),
            Builtin::IMul => TypeBase::binop(LitType::Int),
            Builtin::IDiv => TypeBase::binop(LitType::Int),
            Builtin::IRem => TypeBase::binop(LitType::Int),
            Builtin::INeg => TypeBase::uniop(LitType::Int),
            Builtin::RAdd => TypeBase::binop(LitType::Real),
            Builtin::RSub => TypeBase::binop(LitType::Real),
            Builtin::RMul => TypeBase::binop(LitType::Real),
            Builtin::RDiv => TypeBase::binop(LitType::Real),
            Builtin::BAnd => TypeBase::binop(LitType::Bool),
            Builtin::BOr => TypeBase::binop(LitType::Bool),
            Builtin::BNot => TypeBase::uniop(LitType::Bool),
        }
    }
}

impl<P> Display for TypeBase<P> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeBase::Lit(lit) => write!(f, "{lit}"),
            TypeBase::Var(var, _) => write!(f, "{var}"),
            TypeBase::Cell(cell) => match cell.borrow().deref() {
                TypeCell::Unbound(name, _) => write!(f, "{name}"),
                TypeCell::Link(link) => write!(f, "{link}"),
            },
            TypeBase::Fun(pars, res) => {
                let pars = pars.iter().format(&", ");
                write!(f, "fun({pars}) -> {res}")
            }
            TypeBase::App(cons, args) => {
                let args = args.iter().format(&", ");
                write!(f, "{cons}({args})")
            }
        }
    }
}

impl From<MonoType> for PolyType {
    // todo: maybe use unsafe cast?
    fn from(mty: MonoType) -> Self {
        match mty {
            TypeBase::Lit(lit) => TypeBase::Lit(lit),
            TypeBase::Var(_, _) => unreachable!(),
            TypeBase::Cell(cell) => TypeBase::Cell(cell),
            TypeBase::Fun(pars, res) => TypeBase::Fun(
                pars.into_iter().map(|par| par.into()).collect(),
                Box::new((*res).into()),
            ),
            TypeBase::App(cons, args) => {
                TypeBase::App(cons, args.into_iter().map(|arg| arg.into()).collect())
            }
        }
    }
}

#[derive(Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub enum InferError {
    VarNotInScope,
    CantUnifyLiteralTypes,
    CantUnifyDiffArgLens,
    CantUnifyConstructor,
    CantUnify,
    OccurCheckFailed,
}

pub struct DataCons {}
pub struct FunDecl {}
pub struct DataDecl {}
pub struct TypeDecl {}

type InferResult<T> = Result<T, InferError>;

#[allow(dead_code)]
pub struct Infer {
    val_env: HashMap<Ident, PolyType>,
    cons_env: HashMap<Ident, DataCons>,
    data_env: HashMap<Ident, DataDecl>,
    type_env: HashMap<Ident, TypeDecl>,
    level: usize,
    error: Vec<InferError>,
}

impl Infer {
    pub fn new() -> Infer {
        Infer {
            val_env: HashMap::new(),
            cons_env: HashMap::new(),
            data_env: HashMap::new(),
            type_env: HashMap::new(),
            level: 0,
            error: Vec::new(),
        }
    }
    fn new_cell(&self) -> Rc<RefCell<TypeCell>> {
        let name = Ident::generate('t');
        Rc::new(RefCell::new(TypeCell::Unbound(name, self.level)))
    }

    fn assign(&self, cell: &Rc<RefCell<TypeCell>>, ty: &MonoType) -> InferResult<()> {
        if cell.borrow().is_bound() {
            self.unify(ty, cell.borrow().unwrap_link())
        } else {
            self.update_level(cell, ty)?;
            *cell.borrow_mut() = TypeCell::Link(ty.clone());
            Ok(())
        }
    }

    fn update_level(&self, cell: &Rc<RefCell<TypeCell>>, ty: &MonoType) -> InferResult<()> {
        assert!(!cell.borrow().is_bound());
        match ty {
            TypeBase::Lit(_lit) => Ok(()),
            TypeBase::Var(_var, _) => unreachable!(),
            TypeBase::Cell(cell2) => {
                if Rc::ptr_eq(cell, cell2) {
                    return Err(InferError::OccurCheckFailed);
                }
                match cell2.borrow_mut().deref_mut() {
                    TypeCell::Unbound(_name, level2) => {
                        let level = cell.borrow().unwrap_level();
                        if level < *level2 {
                            *level2 = level;
                        }
                        Ok(())
                    }
                    TypeCell::Link(link) => self.update_level(cell, link),
                }
            }
            TypeBase::Fun(pars, res) => {
                for par in pars {
                    self.update_level(cell, par)?;
                }
                self.update_level(cell, res)
            }
            TypeBase::App(_cons, args) => {
                for arg in args {
                    self.update_level(cell, arg)?;
                }
                Ok(())
            }
        }
    }

    fn unify(&self, ty1: &MonoType, ty2: &MonoType) -> InferResult<()> {
        // println!("unify {:?} ~ {:?}",ty1,ty2);
        match (&ty1, &ty2) {
            (TypeBase::Lit(a), TypeBase::Lit(b)) => {
                if a != b {
                    Err(InferError::CantUnifyLiteralTypes)
                } else {
                    Ok(())
                }
            }
            (TypeBase::Var(_, _), _) | (_, TypeBase::Var(_, _)) => {
                unreachable!()
            }
            (TypeBase::Cell(x), TypeBase::Cell(y)) if Rc::ptr_eq(x, y) => {
                Ok(()) // do nothing
            }
            (TypeBase::Cell(cell), ty) | (ty, TypeBase::Cell(cell)) => self.assign(cell, ty),
            (TypeBase::Fun(pars_a, res_a), TypeBase::Fun(pars_b, res_b)) => {
                if pars_a.len() != pars_b.len() {
                    return Err(InferError::CantUnifyDiffArgLens);
                }
                for (par_a, par_b) in pars_a.iter().zip(pars_b.iter()) {
                    self.unify(par_a, par_b)?;
                }
                self.unify(res_a, res_b)
            }
            (TypeBase::App(cons_a, args_a), TypeBase::App(cons_b, args_b)) => {
                assert_eq!(args_a.len(), args_b.len());
                // todo: type name alias
                if cons_a != cons_b {
                    return Err(InferError::CantUnifyConstructor);
                }
                for (arg_a, arg_b) in args_a.iter().zip(args_b.iter()) {
                    self.unify(arg_a, arg_b)?;
                }
                Ok(())
            }
            (_ty1, _ty2) => Err(InferError::CantUnify),
        }
    }

    fn generalize(&self, mty: &MonoType) -> PolyType {
        let mut map = HashMap::new();
        self.generalize_aux(&mut map, &mty)
    }

    fn generalize_aux(&self, map: &mut HashMap<Ident, Ident>, mty: &MonoType) -> PolyType {
        match mty {
            TypeBase::Lit(lit) => TypeBase::Lit(*lit),
            TypeBase::Var(_, _) => {
                unreachable!()
            }
            TypeBase::Cell(cell) => {
                match cell.borrow().deref() {
                    TypeCell::Unbound(name, level) => {
                        if *level > self.level {
                            if let Some(var) = map.get(&name) {
                                TypeBase::Var(*var, ())
                            } else {
                                // todo: more than 26 type parameters?!!
                                assert!(map.len() < 26);
                                let n = map.len() as u8 + 'a' as u8;
                                let var = Ident::generate(n as char);
                                map.insert(*name, var);
                                TypeBase::Var(var, ())
                            }
                        } else {
                            TypeBase::Cell(cell.clone())
                        }
                    }
                    TypeCell::Link(ty) => self.generalize_aux(map, ty),
                }
            }
            TypeBase::Fun(pars, res) => {
                let pars2 = pars
                    .iter()
                    .map(|par| self.generalize_aux(map, par))
                    .collect();
                let res2 = Box::new(self.generalize_aux(map, res));
                TypeBase::Fun(pars2, res2)
            }
            TypeBase::App(cons, args) => {
                let args2 = args
                    .iter()
                    .map(|arg| self.generalize_aux(map, arg))
                    .collect();
                TypeBase::App(*cons, args2)
            }
        }
    }

    fn instantiate(&self, pty: &PolyType) -> MonoType {
        let mut map = HashMap::new();
        self.instantiate_aux(&mut map, pty)
    }

    fn instantiate_aux(
        &self,
        map: &mut HashMap<Ident, Rc<RefCell<TypeCell>>>,
        pty: &PolyType,
    ) -> MonoType {
        match pty {
            TypeBase::Lit(lit) => TypeBase::Lit(*lit),
            TypeBase::Var(var, _) => {
                if let Some(cell) = map.get(&var) {
                    TypeBase::Cell(cell.clone())
                } else {
                    let cell = self.new_cell();
                    map.insert(*var, cell.clone());
                    TypeBase::Cell(cell)
                }
            }
            TypeBase::Cell(cell) => {
                let ptr = cell.borrow();
                match ptr.deref() {
                    TypeCell::Unbound(_, _) => TypeBase::Cell(cell.clone()),
                    TypeCell::Link(mty) => {
                        // todo: maybe use unsafe cast
                        // `&mty.clone().into()` very slow, very bad!
                        self.instantiate_aux(map, &mty.clone().into())
                    }
                }
            }
            TypeBase::Fun(pars, res) => {
                let pars2 = pars
                    .iter()
                    .map(|par| self.instantiate_aux(map, par))
                    .collect();
                let res2 = Box::new(self.instantiate_aux(map, res));
                TypeBase::Fun(pars2, res2)
            }
            TypeBase::App(cons, args) => {
                let args2 = args
                    .iter()
                    .map(|arg| self.instantiate_aux(map, arg))
                    .collect();
                TypeBase::App(*cons, args2)
            }
        }
    }

    pub fn infer_expr(&mut self, expr: &Expr) -> InferResult<MonoType> {
        match expr {
            Expr::Lit { lit, .. } => Ok(TypeBase::Lit(lit.get_lit_type())),
            Expr::Var { var, .. } => match self.val_env.get(&var) {
                Some(pty) => Ok(self.instantiate(pty)),
                None => Err(InferError::VarNotInScope),
            },
            Expr::Prim { prim, args, .. } => {
                let prim = TypeBase::get_builtin_type(*prim);
                let args = args
                    .iter()
                    .map(|arg| self.infer_expr(arg))
                    .collect::<InferResult<Vec<_>>>()?;
                let res = TypeBase::Cell(self.new_cell());
                let prim_ty = TypeBase::Fun(args, Box::new(res.clone()));
                self.unify(&prim, &prim_ty)?;
                Ok(res)
            }
            Expr::Fun { pars, body, .. } => {
                let pars = pars
                    .iter()
                    .map(|par| {
                        let cell = self.new_cell();
                        self.val_env.insert(*par, TypeBase::Cell(cell.clone()));
                        TypeBase::Cell(cell)
                    })
                    .collect();
                let res = self.infer_expr(body)?;
                Ok(TypeBase::Fun(pars, Box::new(res)))
            }
            Expr::App { func, args, .. } => {
                let func = self.infer_expr(func)?;
                let args = args
                    .iter()
                    .map(|arg| self.infer_expr(arg))
                    .collect::<InferResult<Vec<_>>>()?;
                let res = TypeBase::Cell(self.new_cell());
                let func_ty = TypeBase::Fun(args, Box::new(res.clone()));
                self.unify(&func, &func_ty)?;
                Ok(res)
            }
            Expr::ExtCall {
                func: _, args: _, ..
            } => {
                todo!()
            }
            Expr::Cons {
                cons: _, args: _, ..
            } => {
                todo!()
            }
            Expr::Let {
                bind, expr, cont, ..
            } => {
                self.level += 1;
                let expr = self.infer_expr(expr)?;
                self.level -= 1;
                let expr = self.generalize(&expr);
                self.val_env.insert(*bind, expr);
                let cont = self.infer_expr(cont)?;
                Ok(cont)
            }
            Expr::Case {
                expr: _, rules: _, ..
            } => {
                todo!()
            }
            Expr::Blk {
                decls: _, cont: _, ..
            } => {
                todo!()
            }
        }
    }
}

#[test]
fn type_check_test() {
    use crate::parser::*;
    use crate::renamer::Renamer;
    let string = r#"
let combi = fun(x) => x;
let combk = fun(x,y) => x;
let combkc = fun(x) => fun(y) => x;
let combs = fun(f,g,x) => f(x)(g(x));
combi
"#;

    let mut par = Parser::new(string);
    let expr = parse_expr(&mut par).unwrap();
    println!("{}", expr);
    let mut rnm = Renamer::new();
    let res = rnm.visit_expr(expr);
    println!("{}", res);
    let mut tych = Infer::new();
    tych.infer_expr(&res).unwrap();
    for (k, v) in tych.val_env.iter() {
        println!("{k} : {v}");
    }
}
