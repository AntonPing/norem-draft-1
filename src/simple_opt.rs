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
            MExpr::Stmt { bind, stmt, cont } => {
                let stmt = stmt
                    .args_map(|arg| self.visit_atom(arg))
                    .brchs_map(|brch| self.visit_expr(brch));

                let stmt = match stmt {
                    MStmt::IAdd { arg1, arg2 } => {
                        if let (Some(z), Atom::Int(x), Atom::Int(y)) = (bind, arg1, arg2) {
                            self.map.insert(z, Atom::Int(x + y));
                            return self.visit_expr(*cont);
                        } else {
                            stmt
                        }
                    }
                    MStmt::ISub { arg1, arg2 } => {
                        if let (Some(z), Atom::Int(x), Atom::Int(y)) = (bind, arg1, arg2) {
                            self.map.insert(z, Atom::Int(x - y));
                            return self.visit_expr(*cont);
                        } else {
                            stmt
                        }
                    }
                    MStmt::IMul { arg1, arg2 } => {
                        if let (Some(z), Atom::Int(x), Atom::Int(y)) = (bind, arg1, arg2) {
                            self.map.insert(z, Atom::Int(x * y));
                            return self.visit_expr(*cont);
                        } else {
                            stmt
                        }
                    }
                    MStmt::Move { arg1 } => {
                        if let Some(z) = bind {
                            self.map.insert(z, arg1);
                            return self.visit_expr(*cont);
                        } else {
                            stmt
                        }
                    }
                    MStmt::Alloc { .. } => stmt,
                    MStmt::Load { .. } => stmt,
                    MStmt::Store { .. } => stmt,
                    MStmt::Offset { .. } => stmt,
                    MStmt::Ifte { arg1, brch1, brch2 } => {
                        if let Atom::Bool(p) = arg1 {
                            let cont = self.visit_expr(*cont);
                            if p {
                                return brch1.concat(bind, cont);
                            } else {
                                return brch2.concat(bind, cont);
                            }
                        } else {
                            MStmt::Ifte { arg1, brch1, brch2 }
                        }
                    }
                    MStmt::Switch { arg1, brchs } => {
                        if let Atom::Int(x) = arg1 {
                            let cont = self.visit_expr(*cont);
                            return brchs
                                .into_iter()
                                .nth(x as usize)
                                .unwrap()
                                .concat(bind, cont);
                        } else {
                            MStmt::Switch { arg1, brchs }
                        }
                    }
                };

                let cont = Box::new(self.visit_expr(*cont));
                MExpr::Stmt { bind, stmt, cont }
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
        // substitute until it's constant or no binding
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
            MExpr::Stmt { bind, stmt, cont } => {
                let cont = Box::new(self.visit_expr(*cont));
                let used = if let Some(x) = bind {
                    self.free.remove(&x)
                } else {
                    false
                };
                if !used && stmt.is_pure() {
                    return *cont;
                }
                let stmt = stmt
                    .brchs_map(|brch| self.visit_expr(brch))
                    .args_map(|arg| self.visit_atom(arg));

                MExpr::Stmt { bind, stmt, cont }
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
    use crate::anf::anf_build::*;

    let expr1 = block(vec![
        iadd("x", i(1), i(1)),
        iadd("y", v("x"), i(1)),
        iadd("z", v("y"), i(1)),
        retn(v("z")),
    ]);
    let expr1 = ConstFold::run(expr1);
    let expr2 = block(vec![retn(i(4))]);
    assert_eq!(expr1, expr2);

    let expr1 = block(vec![
        iadd("x", i(1), i(1)),
        _move("y", v("x")),
        call("z", "f", vec![v("x"), v("y")]),
        retn(v("z")),
    ]);
    let expr1 = ConstFold::run(expr1);
    let expr2 = block(vec![call("z", "f", vec![i(2), i(2)]), retn(v("z"))]);
    assert_eq!(expr1, expr2);
}

#[test]
fn dead_elim_test() {
    use crate::anf::anf_build::*;
    let expr1 = block(vec![
        iadd("x", i(1), i(1)),
        iadd("y", v("x"), i(1)),
        iadd("z", v("y"), i(1)),
        retn(v("x")),
    ]);
    let expr1 = DeadElim::run(expr1);
    let expr2 = block(vec![iadd("x", i(1), i(1)), retn(v("x"))]);
    assert_eq!(expr1, expr2);
}
