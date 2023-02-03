use crate::anf::*;
use crate::env_map::EnvMap;
use crate::intern::Unique;

impl MExpr {
    pub fn walk_bind<F: FnMut(Unique) -> Unique>(self, mut f: F) -> MExpr {
        match self {
            MExpr::LetIn { decls, cont } => MExpr::LetIn { decls, cont },
            MExpr::UnOp {
                bind,
                prim,
                arg1,
                cont,
            } => {
                let bind = f(bind);
                MExpr::UnOp {
                    bind,
                    prim,
                    arg1,
                    cont,
                }
            }
            MExpr::BinOp {
                bind,
                prim,
                arg1,
                arg2,
                cont,
            } => {
                let bind = f(bind);
                MExpr::BinOp {
                    bind,
                    prim,
                    arg1,
                    arg2,
                    cont,
                }
            }
            MExpr::Call {
                bind,
                func,
                args,
                cont,
            } => {
                let bind = f(bind);
                MExpr::Call {
                    bind,
                    func,
                    args,
                    cont,
                }
            }
            MExpr::Retn { arg1 } => MExpr::Retn { arg1 },
            MExpr::Alloc { bind, size, cont } => {
                let bind = f(bind);
                MExpr::Alloc { bind, size, cont }
            }
            MExpr::Load {
                bind,
                arg1,
                index,
                cont,
            } => {
                let bind = f(bind);
                MExpr::Load {
                    bind,
                    arg1,
                    index,
                    cont,
                }
            }
            MExpr::Store {
                arg1,
                index,
                arg2,
                cont,
            } => MExpr::Store {
                arg1,
                index,
                arg2,
                cont,
            },
            MExpr::Offset {
                bind,
                arg1,
                index,
                cont,
            } => {
                let bind = f(bind);
                MExpr::Offset {
                    bind,
                    arg1,
                    index,
                    cont,
                }
            }
            MExpr::Ifte {
                bind,
                arg1,
                brch1,
                brch2,
                cont,
            } => {
                let bind = f(bind);
                MExpr::Ifte {
                    bind,
                    arg1,
                    brch1,
                    brch2,
                    cont,
                }
            }
            MExpr::Switch {
                bind,
                arg1,
                brchs,
                dflt,
                cont,
            } => {
                let bind = f(bind);
                MExpr::Switch {
                    bind,
                    arg1,
                    brchs,
                    dflt,
                    cont,
                }
            }
        }
    }

    pub fn walk_arg<F: FnMut(Atom) -> Atom>(self, mut f: F) -> MExpr {
        match self {
            MExpr::LetIn { decls, cont } => MExpr::LetIn { decls, cont },
            MExpr::UnOp {
                bind,
                prim,
                arg1,
                cont,
            } => {
                let arg1 = f(arg1);
                MExpr::UnOp {
                    bind,
                    prim,
                    arg1,
                    cont,
                }
            }
            MExpr::BinOp {
                bind,
                prim,
                arg1,
                arg2,
                cont,
            } => {
                let arg1 = f(arg1);
                let arg2 = f(arg2);
                MExpr::BinOp {
                    bind,
                    prim,
                    arg1,
                    arg2,
                    cont,
                }
            }
            MExpr::Call {
                bind,
                func,
                args,
                cont,
            } => {
                let func = f(func);
                let args = args.into_iter().map(f).collect();
                MExpr::Call {
                    bind,
                    func,
                    args,
                    cont,
                }
            }
            MExpr::Retn { arg1 } => {
                let arg1 = f(arg1);
                MExpr::Retn { arg1 }
            }
            MExpr::Alloc { bind, size, cont } => MExpr::Alloc { bind, size, cont },
            MExpr::Load {
                bind,
                arg1,
                index,
                cont,
            } => {
                let arg1 = f(arg1);
                MExpr::Load {
                    bind,
                    arg1,
                    index,
                    cont,
                }
            }
            MExpr::Store {
                arg1,
                index,
                arg2,
                cont,
            } => {
                let arg1 = f(arg1);
                let arg2 = f(arg2);
                MExpr::Store {
                    arg1,
                    index,
                    arg2,
                    cont,
                }
            }
            MExpr::Offset {
                bind,
                arg1,
                index,
                cont,
            } => {
                let arg1 = f(arg1);
                MExpr::Offset {
                    bind,
                    arg1,
                    index,
                    cont,
                }
            }
            MExpr::Ifte {
                bind,
                arg1,
                brch1,
                brch2,
                cont,
            } => {
                let arg1 = f(arg1);
                MExpr::Ifte {
                    bind,
                    arg1,
                    brch1,
                    brch2,
                    cont,
                }
            }
            MExpr::Switch {
                bind,
                arg1,
                brchs,
                dflt,
                cont,
            } => {
                let arg1 = f(arg1);
                MExpr::Switch {
                    bind,
                    arg1,
                    brchs,
                    dflt,
                    cont,
                }
            }
        }
    }

    pub fn walk_decl<F: FnMut(MDecl) -> MDecl>(self, mut f: F) -> MExpr {
        match self {
            MExpr::LetIn { decls, cont } => {
                let decls = decls.into_iter().map(|decl| f(decl)).collect();
                MExpr::LetIn { decls, cont }
            }
            other => other,
        }
    }

    pub fn walk_brch<F: FnMut(MExpr) -> MExpr>(self, mut f: F) -> MExpr {
        match self {
            MExpr::Ifte {
                bind,
                arg1,
                brch1,
                brch2,
                cont,
            } => {
                let brch1 = Box::new(f(*brch1));
                let brch2 = Box::new(f(*brch2));
                MExpr::Ifte {
                    bind,
                    arg1,
                    brch1,
                    brch2,
                    cont,
                }
            }
            MExpr::Switch {
                bind,
                arg1,
                brchs,
                dflt,
                cont,
            } => {
                let brchs = brchs.into_iter().map(|(i, brch)| (i, f(brch))).collect();
                let dflt = dflt.map(|dflt| Box::new(f(*dflt)));
                MExpr::Switch {
                    bind,
                    arg1,
                    brchs,
                    dflt,
                    cont,
                }
            }
            other => other,
        }
    }

    pub fn walk_cont<F: FnOnce(MExpr) -> MExpr>(self, f: F) -> MExpr {
        match self {
            MExpr::LetIn { decls, cont } => {
                let cont = Box::new(f(*cont));
                MExpr::LetIn { decls, cont }
            }
            MExpr::UnOp {
                bind,
                prim,
                arg1,
                cont,
            } => {
                let cont = Box::new(f(*cont));
                MExpr::UnOp {
                    bind,
                    prim,
                    arg1,
                    cont,
                }
            }
            MExpr::BinOp {
                bind,
                prim,
                arg1,
                arg2,
                cont,
            } => {
                let cont = Box::new(f(*cont));
                MExpr::BinOp {
                    bind,
                    prim,
                    arg1,
                    arg2,
                    cont,
                }
            }
            MExpr::Call {
                bind,
                func,
                args,
                cont,
            } => {
                let cont = Box::new(f(*cont));
                MExpr::Call {
                    bind,
                    func,
                    args,
                    cont,
                }
            }
            MExpr::Retn { arg1 } => MExpr::Retn { arg1 },
            MExpr::Alloc { bind, size, cont } => {
                let cont = Box::new(f(*cont));
                MExpr::Alloc { bind, size, cont }
            }
            MExpr::Load {
                bind,
                arg1,
                index,
                cont,
            } => {
                let cont = Box::new(f(*cont));
                MExpr::Load {
                    bind,
                    arg1,
                    index,
                    cont,
                }
            }
            MExpr::Store {
                arg1,
                index,
                arg2,
                cont,
            } => {
                let cont = Box::new(f(*cont));
                MExpr::Store {
                    arg1,
                    index,
                    arg2,
                    cont,
                }
            }
            MExpr::Offset {
                bind,
                arg1,
                index,
                cont,
            } => {
                let cont = Box::new(f(*cont));
                MExpr::Offset {
                    bind,
                    arg1,
                    index,
                    cont,
                }
            }
            MExpr::Ifte {
                bind,
                arg1,
                brch1,
                brch2,
                cont,
            } => {
                let cont = Box::new(f(*cont));
                MExpr::Ifte {
                    bind,
                    arg1,
                    brch1,
                    brch2,
                    cont,
                }
            }
            MExpr::Switch {
                bind,
                arg1,
                brchs,
                dflt,
                cont,
            } => {
                let cont = Box::new(f(*cont));
                MExpr::Switch {
                    bind,
                    arg1,
                    brchs,
                    dflt,
                    cont,
                }
            }
        }
    }
}

impl MDecl {
    pub fn walk_body<F: FnMut(MExpr) -> MExpr>(self, mut f: F) -> MDecl {
        let MDecl { func, pars, body } = self;
        let body = f(body);
        MDecl { func, pars, body }
    }
}

pub struct Renamer {
    map: EnvMap<Unique, Unique>,
}

impl Renamer {
    pub fn run(expr: MExpr) -> MExpr {
        let mut pass = Renamer::new();
        pass.visit_expr(expr)
    }

    fn new() -> Renamer {
        Renamer { map: EnvMap::new() }
    }

    fn visit_bind(&mut self, bind: Unique) -> Unique {
        let new = bind.rename();
        self.map.insert(bind, new);
        new
    }

    fn visit_arg(&mut self, arg: Atom) -> Atom {
        match arg {
            Atom::Var(x) => Atom::Var(*self.map.get(&x).unwrap_or(&x)),
            other => other,
        }
    }

    fn visit_brch(&mut self, brch: MExpr) -> MExpr {
        self.map.enter_scope();
        let brch = self.visit_expr(brch);
        self.map.leave_scope();
        brch
    }

    fn visit_expr(&mut self, expr: MExpr) -> MExpr {
        match expr {
            MExpr::LetIn { decls, cont } => {
                let decls: Vec<_> = decls
                    .into_iter()
                    .map(|decl| {
                        let MDecl { func, pars, body } = decl;
                        let func = self.visit_bind(func);
                        MDecl { func, pars, body }
                    })
                    .collect();
                let decls = decls
                    .into_iter()
                    .map(|decl| {
                        let MDecl { func, pars, body } = decl;
                        self.map.enter_scope();
                        let pars = pars.into_iter().map(|par| self.visit_bind(par)).collect();
                        let body = self.visit_expr(body);
                        self.map.leave_scope();
                        MDecl { func, pars, body }
                    })
                    .collect();
                let cont = Box::new(self.visit_expr(*cont));
                MExpr::LetIn { decls, cont }
            }
            other => other
                .walk_arg(|arg| self.visit_arg(arg))
                .walk_bind(|bind| self.visit_bind(bind))
                .walk_brch(|brch| self.visit_brch(brch))
                .walk_cont(|cont| self.visit_expr(cont)),
        }
    }
}

impl MExpr {
    pub fn rename(self) -> MExpr {
        Renamer::run(self)
    }
}
