use std::collections::{HashMap, HashSet};

use crate::anf::*;
use crate::intern::Unique;

pub struct ConstFold {
    map: HashMap<Unique, Atom>,
}

impl ConstFold {
    pub fn new() -> ConstFold {
        ConstFold {
            map: HashMap::new(),
        }
    }
    pub fn run(expr: MExpr) -> MExpr {
        let mut pass = ConstFold::new();
        pass.visit_expr(expr)
    }

    fn visit_expr(&mut self, expr: MExpr) -> MExpr {
        match expr {
            MExpr::LetIn { decls, cont } => {
                let decls = decls
                    .into_iter()
                    .map(|decl| self.visit_decl(decl))
                    .collect();
                let cont = Box::new(self.visit_expr(*cont));
                MExpr::LetIn { decls, cont }
            }
            MExpr::Stmt {
                bind,
                prim,
                args,
                cont,
            } => {
                let args: Vec<Atom> = args.into_iter().map(|arg| self.visit_atom(arg)).collect();
                match prim {
                    StmtPrim::IAdd => {
                        if let (Some(z), Atom::Int(x), Atom::Int(y)) = (bind, args[0], args[1]) {
                            self.map.insert(z, Atom::Int(x + y));
                            return self.visit_expr(*cont);
                        }
                    }
                    StmtPrim::IMul => {
                        if let (Some(z), Atom::Int(x), Atom::Int(y)) = (bind, args[0], args[1]) {
                            self.map.insert(z, Atom::Int(x * y));
                            return self.visit_expr(*cont);
                        }
                    }
                    StmtPrim::Move => {
                        if let (Some(z), x) = (bind, args[0]) {
                            self.map.insert(z, x);
                            return self.visit_expr(*cont);
                        }
                    }
                    // todo: other primitives
                    _ => {
                        // do nothing
                    }
                }
                let cont = Box::new(self.visit_expr(*cont));
                MExpr::Stmt {
                    prim,
                    args,
                    bind,
                    cont,
                }
            }
            MExpr::Brch { prim, args, conts } => {
                let args: Vec<Atom> = args.into_iter().map(|arg| self.visit_atom(arg)).collect();

                match prim {
                    BrchPrim::Ifte => {
                        if let Atom::Bool(x) = args[0] {
                            let [trbr, flbr]: [MExpr; 2] = conts.try_into().unwrap();
                            if x {
                                return self.visit_expr(trbr);
                            } else {
                                return self.visit_expr(flbr);
                            }
                        }
                    }
                    BrchPrim::Switch => {
                        if let Atom::Int(x) = args[0] {
                            let cont = conts.into_iter().nth(x as usize).unwrap();
                            return self.visit_expr(cont);
                        }
                    }
                    // todo: more branch reduction
                    _ => {
                        // do nothing
                    }
                }

                let conts = conts
                    .into_iter()
                    .map(|cont| self.visit_expr(cont))
                    .collect();

                MExpr::Brch { prim, args, conts }
            }
            MExpr::Call {
                bind,
                func,
                args,
                cont,
            } => {
                let func = self.visit_call_func(func);
                let args = args.into_iter().map(|arg| self.visit_atom(arg)).collect();
                let cont = Box::new(self.visit_expr(*cont));
                MExpr::Call {
                    func,
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

    fn visit_decl(&mut self, decl: MDecl) -> MDecl {
        let MDecl { func, pars, body } = decl;
        let body = self.visit_expr(body);
        MDecl { func, pars, body }
    }

    fn visit_call_func(&mut self, func: CallFunc) -> CallFunc {
        match func {
            CallFunc::Intern(var) => {
                let atom = self.visit_atom(Atom::Var(var));
                let var = atom.unwrap_variable();
                CallFunc::Intern(var)
            }
            CallFunc::Extern(var) => CallFunc::Extern(var),
        }
    }

    fn visit_atom(&mut self, atom: Atom) -> Atom {
        // substitute until constant or no binding
        let mut atom = atom;
        loop {
            if let Atom::Var(sym) = atom {
                if let Some(res) = self.map.get(&sym) {
                    atom = *res
                } else {
                    return atom;
                }
            } else {
                return atom;
            }
        }
    }
}

/*
    Dead-Code Elimination Pass
*/

pub struct DeadElim {
    free: HashSet<Unique>,
}

fn fix_point<T>(set: HashSet<T>, graph: HashMap<T, HashSet<T>>) -> HashSet<T>
where
    T: std::hash::Hash + Copy + Eq,
{
    let mut set = set;
    let mut new = HashSet::new();
    loop {
        for key in &set {
            if let Some(val) = graph.get(&key) {
                for x in val {
                    if !set.contains(&x) {
                        new.insert(*x);
                    }
                }
            }
        }
        if new.is_empty() {
            return set;
        } else {
            set.extend(new.drain())
        }
    }
}

impl DeadElim {
    pub fn new() -> DeadElim {
        DeadElim {
            free: HashSet::new(),
        }
    }
    pub fn run(expr: MExpr) -> MExpr {
        let mut pass = DeadElim::new();
        pass.visit_expr(expr)
    }

    fn visit_expr(&mut self, expr: MExpr) -> MExpr {
        match expr {
            MExpr::LetIn { decls, cont } => {
                let func_names: Vec<Unique> = decls.iter().map(|decl| decl.func).collect();

                let cont = Box::new(self.visit_expr(*cont));
                let used_set: HashSet<Unique> = func_names
                    .iter()
                    .filter(|func| self.free.remove(func))
                    .copied()
                    .collect();

                // collect for each func decl for reachable reference
                let mut graph: HashMap<Unique, HashSet<Unique>> = HashMap::new();
                let decls: Vec<MDecl> = decls
                    .into_iter()
                    .map(|decl| {
                        let decl = self.visit_decl(decl);
                        let used_set = func_names
                            .iter()
                            .filter(|func| self.free.remove(&func))
                            .copied()
                            .collect();
                        graph.insert(decl.func, used_set);
                        decl
                    })
                    .collect();

                // calculate the fix-point set, all functions that not in this set are unused functions
                let used_set = fix_point(used_set, graph);

                // delete all unused functions
                let decls: Vec<MDecl> = decls
                    .into_iter()
                    .filter(|decl| used_set.contains(&decl.func))
                    .collect();

                if decls.is_empty() {
                    *cont
                } else {
                    MExpr::LetIn { decls, cont }
                }
            }
            MExpr::Stmt {
                bind,
                prim,
                args,
                cont,
            } => {
                let cont = Box::new(self.visit_expr(*cont));
                let used = if let Some(x) = bind {
                    self.free.remove(&x)
                } else {
                    false
                };
                if !used && prim.is_pure() {
                    return *cont;
                }
                let args: Vec<Atom> = args.into_iter().map(|arg| self.visit_atom(arg)).collect();
                MExpr::Stmt {
                    bind,
                    prim,
                    args,
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
                func,
                args,
                cont,
            } => {
                let cont = Box::new(self.visit_expr(*cont));
                let _used = if let Some(x) = bind {
                    self.free.remove(&x)
                } else {
                    false
                };
                if false
                /* !used && func.is_pure() */
                {
                    // todo: eliminate pure function call if the result is never used
                    // can't do it now because we don't have that information yet
                    return *cont;
                }
                let func = self.visit_call_func(func);
                let args = args.into_iter().map(|arg| self.visit_atom(arg)).collect();
                MExpr::Call {
                    bind,
                    func,
                    args,
                    cont,
                }
            }
            MExpr::Retn { atom } => {
                let atom = self.visit_atom(atom);
                MExpr::Retn { atom }
            }
        }
    }

    fn visit_call_func(&mut self, func: CallFunc) -> CallFunc {
        match func {
            CallFunc::Intern(var) => {
                let atom = self.visit_atom(Atom::Var(var));
                let var = atom.unwrap_variable();
                CallFunc::Intern(var)
            }
            CallFunc::Extern(var) => CallFunc::Extern(var),
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
fn const_fold_test() {
    use crate::anf::BrchPrim::*;
    use crate::anf::StmtPrim::*;
    use crate::{atom, decls, expr, ident};

    let expr1 = expr! {
        stmt x = IAdd, 1, 1;
        stmt y = IAdd, x, 1;
        stmt z = IAdd, y, 1;
        retn z;
    };
    let expr1 = ConstFold::run(expr1);
    let expr2 = expr! {
        retn 4;
    };
    assert_eq!(expr1, expr2);

    let expr1 = expr! {
        stmt x = IAdd, 1, 1;
        stmt y = Move, x;
        call z = f (x, y);
        retn z;
    };
    let expr1 = ConstFold::run(expr1);
    let expr2 = expr! {
        call z = f (2, 2);
        retn z;
    };
    assert_eq!(expr1, expr2);
}

#[test]
#[allow(unused_imports)]
fn dead_elim_test() {
    use crate::anf::BrchPrim::*;
    use crate::anf::StmtPrim::*;
    use crate::{atom, decls, expr, ident};

    let expr1 = expr! {
        stmt x = IAdd, 1, 1;
        stmt y = IAdd, x, 1;
        stmt z = IAdd, y, 1;
        retn x;
    };
    let expr1 = DeadElim::run(expr1);
    let expr2 = expr! {
        stmt x = IAdd, 1, 1;
        retn x;
    };
    assert_eq!(expr1, expr2);
}
