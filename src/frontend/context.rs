use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::fmt;

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
    pub fn new_type() -> TypeBase {
        TypeBase::Var(Ident::generate('t'))
    }
    pub fn rename_type(ident: &Ident) -> TypeBase {
        TypeBase::Var(ident.uniquify())
    }
    pub fn subst(&self, map: &HashMap<Ident, TypeBase>) -> TypeBase {
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
    pub fn is_free(&self, var: &Ident) -> bool {
        match self {
            TypeBase::Lit(_) => false,
            TypeBase::Gen(_) => false,
            TypeBase::Var(var2) => *var == *var2,
            TypeBase::Fun(pars, res) => pars.iter().any(|par| par.is_free(var)) || res.is_free(var),
            TypeBase::App(cons, args) => (*cons == *var) || args.iter().any(|arg| arg.is_free(var)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Function {
    pub name: Ident,
    pub gens: Vec<Ident>,
    pub pars: Vec<(Ident, TypeBase)>,
    pub res: TypeBase,
}

#[derive(Clone, Debug)]
pub struct DataType {
    pub name: Ident,
    pub pars: Vec<Ident>,
    // contains these constructors
    pub cons: Vec<Ident>,
}

#[derive(Clone, Debug)]
pub struct Constructor {
    pub name: Ident,
    pub flds: Vec<TypeBase>,
    // belongs to which DataDecl
    pub data: Ident,
}

#[derive(Clone, Debug)]
pub struct TypeAlias {
    pub name: Ident,
    pub pars: Vec<Ident>,
    pub typ: TypeBase,
}

#[derive(Clone, Debug)]
pub struct External {
    pub name: InternStr,
    pub gens: Vec<Ident>,
    pub typ: TypeBase,
}

#[derive(Clone, Debug)]
pub struct Context {
    pub val_env: HashMap<Ident, TypeBase>,
    pub func_env: HashMap<Ident, Function>,
    pub data_env: HashMap<Ident, DataType>,
    pub cons_env: HashMap<Ident, Constructor>,
    pub type_env: HashMap<Ident, TypeAlias>,
    pub extn_env: HashMap<InternStr, External>,
    pub gen_set: HashSet<Ident>,
}

impl Context {
    pub fn new() -> Context {
        Context {
            val_env: HashMap::new(),
            func_env: HashMap::new(),
            cons_env: HashMap::new(),
            data_env: HashMap::new(),
            type_env: HashMap::new(),
            extn_env: HashMap::new(),
            gen_set: HashSet::new(),
        }
    }

    pub fn is_generic(&self, ident: &Ident) -> bool {
        self.gen_set.contains(ident)
    }

    pub fn lookup_val(&self, var: &Ident) -> TypeBase {
        self.val_env[var].clone()
    }

    pub fn instantiate_func(&self, func: &Ident) -> TypeBase {
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

    pub fn instantiate_cons(&self, cons: &Ident) -> (Vec<TypeBase>, TypeBase) {
        let cons_decl = self.cons_env[cons].clone();
        let data_decl = self.data_env[&cons_decl.data].clone();
        if data_decl.pars.is_empty() {
            let pars = cons_decl.flds.clone();
            let app = TypeBase::App(cons_decl.data, Vec::new());
            (pars, app)
        } else {
            let map: HashMap<_, _> = data_decl
                .pars
                .iter()
                .map(|x| (*x, TypeBase::rename_type(x)))
                .collect();
            let pars = cons_decl.flds.iter().map(|typ| typ.subst(&map)).collect();
            let args = data_decl.pars.iter().map(|x| map[x].clone()).collect();
            let app = TypeBase::App(cons_decl.data, args);
            (pars, app)
        }
    }

    pub fn instantiate_extn(&self, name: &InternStr) -> TypeBase {
        let decl = self.extn_env[name].clone();
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

    pub fn get_cons_index(&self, cons: &Ident) -> usize {
        let data = self.cons_env[cons].data;
        self.data_env[&data]
            .cons
            .iter()
            .find_position(|cons2| **cons2 == *cons)
            .unwrap()
            .0
    }
}
