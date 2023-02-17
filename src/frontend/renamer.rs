use super::*;
use crate::utils::env_map::EnvMap;
use std::collections::HashSet;

pub struct Renamer {
    /// map a dummy identifier to an unique Identifier
    val_map: EnvMap<Ident, Ident>,
    typ_map: EnvMap<Ident, Ident>,
    cons_map: EnvMap<Ident, Ident>,
    ext_set: HashSet<InternStr>,
    error: Vec<RenameError>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RenameError {
    UnboundedValueVariable(Span, Ident),
    UnboundedTypeVariable(Span, Ident),
    UnboundedConstructorVariable(Span, Ident),
    UndefinedExternalFunction(Span, InternStr),
    MultipuleDefinition(Span, Ident),
    MultipuleExternalDefinition(Span, InternStr),
}

impl Renamer {
    pub fn new() -> Renamer {
        Renamer {
            val_map: EnvMap::new(),
            typ_map: EnvMap::new(),
            cons_map: EnvMap::new(),
            ext_set: HashSet::new(),
            error: Vec::new(),
        }
    }

    pub fn run(expr: &mut Expr) -> Result<(), Vec<RenameError>> {
        let mut pass = Renamer::new();
        pass.visit_expr(expr);
        if pass.error.is_empty() {
            Ok(())
        } else {
            Err(pass.error)
        }
    }

    fn enter_scope(&mut self) {
        self.val_map.enter_scope();
        self.typ_map.enter_scope();
        self.cons_map.enter_scope();
    }

    fn leave_scope(&mut self) {
        self.val_map.leave_scope();
        self.typ_map.leave_scope();
        self.cons_map.leave_scope();
    }

    fn intro_val_var(&mut self, var: &mut Ident) {
        let ident = var.uniquify();
        self.val_map.insert(*var, ident);
        *var = ident;
    }

    fn intro_typ_var(&mut self, var: &mut Ident) {
        let ident = var.uniquify();
        self.typ_map.insert(*var, ident);
        *var = ident;
    }

    fn intro_cons_var(&mut self, var: &mut Ident) {
        let ident = var.uniquify();
        self.cons_map.insert(*var, ident);
        *var = ident;
    }

    fn lookup_val_var(&mut self, ident: &Ident) -> Option<Ident> {
        self.val_map.get(&ident).copied()
    }

    fn lookup_typ_var(&mut self, ident: &Ident) -> Option<Ident> {
        self.typ_map.get(&ident).copied()
    }

    fn lookup_cons_var(&mut self, ident: &Ident) -> Option<Ident> {
        self.cons_map.get(&ident).copied()
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::Lit { .. } => {}
            Expr::Var { var, span } => {
                if let Some(var2) = self.lookup_val_var(var) {
                    *var = var2;
                } else {
                    self.error
                        .push(RenameError::UnboundedValueVariable(*span, *var));
                }
            }
            Expr::Prim { prim: _, args, .. } => {
                args.iter_mut().for_each(|arg| self.visit_expr(arg));
            }
            Expr::Fun { pars, body, .. } => {
                self.enter_scope();
                pars.iter_mut().for_each(|par| {
                    assert!(par.is_dummy());
                    self.intro_val_var(par)
                });
                self.visit_expr(&mut *body);
                self.leave_scope();
            }
            Expr::App { func, args, .. } => {
                self.visit_expr(&mut *func);
                args.iter_mut().for_each(|arg| self.visit_expr(arg));
            }
            Expr::ExtCall { func, args, span } => {
                if !self.ext_set.contains(&func) {
                    self.error
                        .push(RenameError::UndefinedExternalFunction(*span, *func))
                }
                args.iter_mut().for_each(|arg| self.visit_expr(arg));
            }
            Expr::Cons { cons, args, span } => {
                if let Some(cons2) = self.lookup_cons_var(cons) {
                    *cons = cons2;
                } else {
                    self.error
                        .push(RenameError::UnboundedConstructorVariable(*span, *cons));
                }
                args.iter_mut().for_each(|arg| self.visit_expr(arg));
            }
            Expr::Begin { block, .. } => {
                self.visit_block(&mut *block);
            }
            Expr::Case { expr, rules, .. } => {
                self.visit_expr(&mut *expr);
                rules.iter_mut().for_each(|rule| self.visit_rule(rule));
            }
            Expr::Ifte {
                cond, trbr, flbr, ..
            } => {
                self.visit_expr(&mut *cond);
                self.visit_expr(&mut *trbr);
                self.visit_expr(&mut *flbr);
            }
            Expr::Letrec { decls, block, span } => {
                self.enter_scope();
                let mut name_set: HashSet<Ident> = HashSet::new();
                for decl in decls.iter_mut() {
                    assert!(decl.get_name().is_dummy());
                    match decl {
                        Decl::Func { name, .. } => {
                            if name_set.contains(name) {
                                self.error
                                    .push(RenameError::MultipuleDefinition(*span, *name));
                            } else {
                                name_set.insert(*name);
                            }
                            self.intro_val_var(name);
                        }
                        Decl::Data { name, vars, .. } => {
                            if name_set.contains(name) {
                                self.error
                                    .push(RenameError::MultipuleDefinition(*span, *name));
                            } else {
                                name_set.insert(*name);
                            }
                            self.intro_typ_var(name);
                            for var in vars {
                                self.intro_cons_var(&mut var.cons);
                            }
                        }
                        Decl::Type { name, .. } => {
                            if name_set.contains(name) {
                                self.error
                                    .push(RenameError::MultipuleDefinition(*span, *name));
                            } else {
                                name_set.insert(*name);
                            }
                            self.intro_typ_var(name);
                        }
                        Decl::Extern { name, span, .. } => {
                            if self.ext_set.contains(&name) {
                                self.error
                                    .push(RenameError::MultipuleExternalDefinition(*span, *name));
                            }
                            self.ext_set.insert(*name);
                        }
                    }
                }
                decls.iter_mut().for_each(|decl| self.visit_decl(decl));
                self.visit_block(&mut *block);
                self.leave_scope();
            }
        }
    }

    fn visit_block(&mut self, block: &mut Block) {
        let Block { stmts, retn, .. } = block;
        self.enter_scope();
        stmts.iter_mut().for_each(|stmt| self.visit_stmt(stmt));
        retn.as_mut().map(|retn| self.visit_expr(retn));
        self.leave_scope();
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::Bind { bind, expr, .. } => {
                self.visit_expr(expr);
                self.intro_val_var(bind);
            }
            Stmt::Do { expr, .. } => {
                self.visit_expr(expr);
            }
        }
    }

    fn visit_rule(&mut self, rule: &mut Rule) {
        let Rule { patn, body, .. } = rule;
        self.enter_scope();
        self.visit_patn(patn);
        self.visit_expr(body);
        self.leave_scope();
    }

    fn visit_patn(&mut self, patn: &mut Pattern) {
        match patn {
            Pattern::Var { var, .. } => {
                self.intro_val_var(var);
            }
            Pattern::Lit { .. } => {}
            Pattern::Cons { cons, pars, span } => {
                if let Some(cons2) = self.lookup_cons_var(cons) {
                    *cons = cons2;
                } else {
                    self.error
                        .push(RenameError::UnboundedConstructorVariable(*span, *cons));
                }
                pars.iter_mut().for_each(|par| self.visit_patn(par));
            }
            Pattern::Wild { .. } => {}
        }
    }

    fn visit_decl(&mut self, decl: &mut Decl) {
        match decl {
            Decl::Func {
                name: _,
                gens,
                pars,
                res,
                body,
                ..
            } => {
                self.enter_scope();
                // self.intro_val_var(name);
                for gen in gens {
                    self.intro_typ_var(gen);
                }
                pars.iter_mut().for_each(|(par, typ)| {
                    self.intro_val_var(par);
                    self.visit_type(typ);
                });
                self.visit_type(res);
                self.visit_expr(&mut *body);
                self.leave_scope();
            }
            Decl::Data {
                name: _,
                pars,
                vars,
                ..
            } => {
                self.enter_scope();
                // self.intro_typ_var(name);
                pars.iter_mut().for_each(|par| self.intro_typ_var(par));
                vars.iter_mut().for_each(|var| self.visit_varient(var));
                self.leave_scope();
            }
            Decl::Type {
                name: _, pars, typ, ..
            } => {
                self.enter_scope();
                // self.intro_typ_var(name);
                pars.iter_mut().for_each(|par| self.intro_typ_var(par));
                self.visit_type(typ);
                self.leave_scope();
            }
            Decl::Extern {
                name: _, pars, typ, ..
            } => {
                self.enter_scope();
                pars.iter_mut().for_each(|par| self.intro_typ_var(par));
                self.visit_type(typ);
                self.leave_scope();
            }
        }
    }

    fn visit_varient(&mut self, var: &mut Varient) {
        let Varient { cons: _, pars, .. } = var;
        // self.intro_cons_var(cons);
        pars.iter_mut().for_each(|par| self.visit_type(par));
    }

    fn visit_type(&mut self, typ: &mut Type) {
        match typ {
            Type::Lit { .. } => {}
            Type::Var { var, span } => {
                if let Some(var2) = self.lookup_typ_var(var) {
                    *var = var2;
                } else {
                    self.error
                        .push(RenameError::UnboundedTypeVariable(*span, *var));
                }
            }
            Type::Fun { pars, res, .. } => {
                pars.iter_mut().for_each(|par| self.visit_type(par));
                self.visit_type(&mut *res);
            }
            Type::App { cons, args, span } => {
                if let Some(cons2) = self.lookup_typ_var(cons) {
                    *cons = cons2;
                } else {
                    self.error
                        .push(RenameError::UnboundedTypeVariable(*span, *cons));
                }
                args.iter_mut().for_each(|arg| self.visit_type(arg));
            }
        }
    }
}

#[test]
fn renamer_test() {
    use super::parser::*;
    let string = r#"
letrec
    extern print_int : fun(Int) -> ();
    type My-Int = Int;
    type Option-Int = Option[Int];
    data Option[T] =
    | Some(T)
    | None
    end
    fun add1(x: Int): Int = @iadd(x, 1)
    fun add2(x: Int): Int = begin
        #print_int(x);
        let y: Int = @iadd(x,1);
        #print_int(zzzzz);
        @iadd(y,1)
    end
    fun const-3[T](x: T): Int = begin
        3
    end
    fun option-add1(x: Option[Int]): Option[Int] =
        case x of
        | Some(y) => Some(@iadd(x,1))
        | None => None
        end
in
    @isub(add1(42), 1)
end
"#;

    let mut par = Parser::new(string);
    let mut expr = parse_expr(&mut par).unwrap();
    // println!("{}", expr);
    let err = Renamer::run(&mut expr);
    // println!("{:?}", err);
    assert!(err.is_err());
    assert_eq!(err.as_ref().unwrap_err().len(), 1);
    assert!(matches!(
        err.unwrap_err()[0],
        RenameError::UnboundedValueVariable(_, _)
    ));
}
