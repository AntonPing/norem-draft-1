use crate::ast::*;
use crate::env_map::EnvMap;
use crate::intern::{InternStr, Unique};
use crate::position::Span;

pub struct Renamer {
    /// map a `InternStr` to an `Unique` Identifier
    val_map: EnvMap<InternStr, Unique>,
    typ_map: EnvMap<InternStr, Unique>,
    error: Vec<RenameError>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum RenameError {
    UnboundedValueVariable(Span, InternStr),
    UnboundedTypeVariable(Span, InternStr),
    MultipuleDefinition(Span, InternStr),
}

pub struct UnboundedError(InternStr);

impl Renamer {
    pub fn new() -> Renamer {
        Renamer {
            val_map: EnvMap::new(),
            typ_map: EnvMap::new(),
            error: Vec::new(),
        }
    }

    fn enter_scope(&mut self) {
        self.val_map.enter_scope();
        self.typ_map.enter_scope();
    }

    fn leave_scope(&mut self) {
        self.val_map.leave_scope();
        self.typ_map.leave_scope();
    }

    fn intro_val_var(&mut self, ident: InternStr) -> Unique {
        let uniq = ident.to_unique();
        self.val_map.insert(ident, uniq);
        uniq
    }

    fn intro_typ_var(&mut self, ident: InternStr) -> Unique {
        let uniq = ident.to_unique();
        self.typ_map.insert(ident, uniq);
        uniq
    }

    fn lookup_val_var(&mut self, ident: InternStr) -> Option<Unique> {
        self.val_map.get(&ident).copied()
    }

    fn lookup_typ_var(&mut self, ident: InternStr) -> Option<Unique> {
        self.typ_map.get(&ident).copied()
    }

    pub fn visit_expr(&mut self, expr: Expr<InternStr>) -> Expr<Unique> {
        match expr {
            Expr::Lit { lit, span } => Expr::Lit { lit, span },
            Expr::Var { var, span } => {
                let var = self.lookup_val_var(var).unwrap_or_else(|| {
                    self.error
                        .push(RenameError::UnboundedValueVariable(span, var));
                    var.to_unique()
                });
                Expr::Var { var, span }
            }
            Expr::Prim { prim, args, span } => {
                let args = args.into_iter().map(|arg| self.visit_expr(arg)).collect();
                Expr::Prim { prim, args, span }
            }
            Expr::Fun { pars, body, span } => {
                self.enter_scope();
                let pars: Vec<Unique> = pars
                    .into_iter()
                    .map(|par| self.intro_val_var(par))
                    .collect();
                let body = Box::new(self.visit_expr(*body));
                self.leave_scope();
                Expr::Fun { pars, body, span }
            }
            Expr::App { func, args, span } => {
                let func = Box::new(self.visit_expr(*func));
                let args = args.into_iter().map(|arg| self.visit_expr(arg)).collect();
                Expr::App { func, args, span }
            }
            Expr::Let {
                bind,
                expr,
                cont,
                span,
            } => {
                let expr = Box::new(self.visit_expr(*expr));
                self.enter_scope();
                let bind = self.intro_val_var(bind);
                let cont = Box::new(self.visit_expr(*cont));
                self.leave_scope();
                Expr::Let {
                    bind,
                    expr,
                    cont,
                    span,
                }
            }
            Expr::Case { expr, rules, span } => {
                let expr = Box::new(self.visit_expr(*expr));
                let rules = rules
                    .into_iter()
                    .map(|rule| self.visit_rule(rule))
                    .collect();
                Expr::Case { expr, rules, span }
            }
            Expr::Blk { decls, cont, span } => {
                self.enter_scope();
                // todo: multiple definition error
                for decl in &decls {
                    match decl {
                        Decl::Func { name, .. } => {
                            self.intro_val_var(*name);
                        }
                        Decl::Data { name, vars, .. } => {
                            self.intro_typ_var(*name);
                            for var in vars {
                                self.intro_val_var(var.cons);
                            }
                        }
                        Decl::Type { name, .. } => {
                            self.intro_typ_var(*name);
                        }
                    }
                }
                let decls = decls
                    .into_iter()
                    .map(|decl| self.visit_decl(decl))
                    .collect();
                let cont = Box::new(self.visit_expr(*cont));
                self.leave_scope();
                Expr::Blk { decls, cont, span }
            }
        }
    }

    pub fn visit_rule(&mut self, rule: Rule<InternStr>) -> Rule<Unique> {
        let Rule { patn, body, span } = rule;
        self.enter_scope();
        let patn = self.visit_patn(patn);
        let body = self.visit_expr(body);
        self.leave_scope();
        Rule { patn, body, span }
    }

    pub fn visit_patn(&mut self, patn: Pattern<InternStr>) -> Pattern<Unique> {
        match patn {
            Pattern::Var { var, span } => {
                let var = self.intro_val_var(var);
                Pattern::Var { var, span }
            }
            Pattern::Lit { lit, span } => Pattern::Lit { lit, span },
            Pattern::Cons { cons, pars, span } => {
                let cons = self.lookup_val_var(cons).unwrap_or_else(|| {
                    self.error
                        .push(RenameError::UnboundedValueVariable(span, cons));
                    cons.to_unique()
                });
                let pars = pars.into_iter().map(|par| self.visit_patn(par)).collect();
                Pattern::Cons { cons, pars, span }
            }
            Pattern::Wild { span } => Pattern::Wild { span },
        }
    }

    pub fn visit_decl(&mut self, decl: Decl<InternStr>) -> Decl<Unique> {
        match decl {
            Decl::Func {
                name,
                pars,
                body,
                span,
            } => {
                self.enter_scope();
                let name = self.lookup_val_var(name).unwrap_or_else(|| {
                    self.error
                        .push(RenameError::UnboundedValueVariable(span, name));
                    name.to_unique()
                });
                let pars = pars
                    .into_iter()
                    .map(|par| self.intro_val_var(par))
                    .collect();
                let body = Box::new(self.visit_expr(*body));
                self.leave_scope();
                Decl::Func {
                    name,
                    pars,
                    body,
                    span,
                }
            }
            Decl::Data {
                name,
                pars,
                vars,
                span,
            } => {
                self.enter_scope();
                let name = self.lookup_typ_var(name).unwrap();
                let pars = pars
                    .into_iter()
                    .map(|par| self.intro_typ_var(par))
                    .collect();
                let vars = vars
                    .into_iter()
                    .map(|var| self.visit_varient(var))
                    .collect();
                self.leave_scope();
                Decl::Data {
                    name,
                    pars,
                    vars,
                    span,
                }
            }
            Decl::Type {
                name,
                pars,
                typ,
                span,
            } => {
                self.enter_scope();
                let name = self.lookup_typ_var(name).unwrap();
                let pars = pars
                    .into_iter()
                    .map(|par| self.intro_typ_var(par))
                    .collect();
                let typ = self.visit_type(typ);
                self.leave_scope();
                Decl::Type {
                    name,
                    pars,
                    typ,
                    span,
                }
            }
        }
    }

    pub fn visit_varient(&mut self, var: Varient<InternStr>) -> Varient<Unique> {
        let Varient { cons, pars, span } = var;
        let cons = self.lookup_val_var(cons).unwrap();
        let pars = pars.into_iter().map(|par| self.visit_type(par)).collect();
        Varient { cons, pars, span }
    }

    pub fn visit_type(&mut self, typ: Type<InternStr>) -> Type<Unique> {
        match typ {
            Type::Lit { lit, span } => Type::Lit { lit, span },
            Type::Var { var, span } => {
                let var = self.lookup_typ_var(var).unwrap_or_else(|| {
                    self.error
                        .push(RenameError::UnboundedTypeVariable(span, var));
                    var.to_unique()
                });
                Type::Var { var, span }
            }
            Type::Fun { pars, res, span } => {
                let pars = pars.into_iter().map(|par| self.visit_type(par)).collect();
                let res = Box::new(self.visit_type(*res));
                Type::Fun { pars, res, span }
            }
            Type::App { cons, args, span } => {
                let cons = self.lookup_typ_var(cons).unwrap_or_else(|| {
                    self.error
                        .push(RenameError::UnboundedTypeVariable(span, cons));
                    cons.to_unique()
                });
                let args = args.into_iter().map(|arg| self.visit_type(arg)).collect();
                Type::App { cons, args, span }
            }
        }
    }
}

#[test]
fn renamer_test() {
    use crate::parser::*;
    let string = r#"
begin
    type My-Int = Int;
    type Option-Int = Option[Int];
    data Option[T] =
    | Some(T)
    | None
    end
    fun add1(x) => @iadd(x, 1)
    fun add2(x) =>
        let y = @iadd(x,1);
        @iadd(z,1)
    fun const-3(x) => {
        let y = @iadd(x,1);
        @iadd(y,1)
    }
    fun option-add1(x) =>
        case x of
        | Some(y) => { Some(@iadd(x,1)) }
        | None => { None }
        end
in
    @isub(add1(42), 1)
end
"#;

    let mut par = Parser::new(string);
    let expr = parse_expr(&mut par).unwrap();
    // println!("{}", expr);
    let mut rnm = Renamer::new();
    let _res = rnm.visit_expr(expr);
    // println!("{}", _res);

    assert_eq!(rnm.error.len(), 1);
    match rnm.error[0] {
        RenameError::UnboundedValueVariable(span, var) => {
            // line 11: @iadd(z,1) <- here z not bound!
            assert_eq!(span.start.row, 11);
            assert_eq!(format!("{}", var), "z");
        }
        _ => {
            panic!("test failed!");
        }
    }
}
