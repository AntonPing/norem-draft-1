use crate::ast::*;
use itertools::{self, Itertools};
use std::fmt::{self, Debug, Display};

pub struct INDT;
pub struct DEDT;
pub struct NWLN;

static mut INDT_LEVEL: usize = 0;

impl Display for INDT {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            INDT_LEVEL += 1;
        }
        Ok(())
    }
}

impl Display for DEDT {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unsafe {
            INDT_LEVEL -= 1;
        }
        Ok(())
    }
}

impl Display for NWLN {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "")?;
        write!(f, "{:width$}", "", width = unsafe { INDT_LEVEL * 2 })
    }
}

impl Debug for INDT {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self)
    }
}

impl Debug for DEDT {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self)
    }
}

impl Debug for NWLN {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", self)
    }
}

impl Display for LitVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LitVal::Int(x) => write!(f, "{x}"),
            LitVal::Real(x) => write!(f, "{x}"),
            LitVal::Bool(x) => write!(f, "{x}"),
            LitVal::Char(x) => write!(f, "{x}"),
        }
    }
}

impl Display for LitType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Builtin::IAdd => write!(f, "iadd"),
            Builtin::ISub => write!(f, "isub"),
            Builtin::IMul => write!(f, "imul"),
            Builtin::IDiv => write!(f, "idiv"),
            Builtin::IRem => write!(f, "irem"),
            Builtin::INeg => write!(f, "ineg"),
            Builtin::RAdd => write!(f, "radd"),
            Builtin::RSub => write!(f, "rsub"),
            Builtin::RMul => write!(f, "rmul"),
            Builtin::RDiv => write!(f, "rdiv"),
            Builtin::BAnd => write!(f, "band"),
            Builtin::BOr => write!(f, "bor"),
            Builtin::BNot => write!(f, "bnot"),
        }
    }
}

impl<Ident: Display> Display for Expr<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Lit { lit, .. } => {
                write!(f, "{lit}")
            }
            Expr::Var { var, .. } => {
                write!(f, "{var}")
            }
            Expr::Prim { prim, args, .. } => {
                let args = args.iter().format(&", ");
                write!(f, "@{prim}({args})")
            }
            Expr::Fun { pars, body, .. } => {
                let pars = pars.iter().format(&", ");
                write!(f, "fn ({pars}) {{{INDT}{NWLN}{body}{DEDT}{NWLN}}}")
            }
            Expr::App { func, args, .. } => {
                let args = args.iter().format(&", ");
                write!(f, "{func}({args})")
            }
            Expr::Let {
                bind, expr, cont, ..
            } => {
                write!(f, "let {bind} = {expr};{NWLN}{cont}")
            }
            Expr::Blk { decls, cont, .. } => {
                if decls.is_empty() {
                    write!(f, "begin{INDT}{NWLN}{cont}{DEDT}{NWLN}end")
                } else {
                    write!(f, "begin{INDT}")?;
                    for decl in decls {
                        write!(f, "{NWLN}{decl}")?;
                    }
                    write!(f, "{DEDT}{NWLN}in{INDT}{NWLN}{cont}{DEDT}{NWLN}end")
                }
            }
            Expr::Case { expr, rules, .. } => {
                // Void can't be defined by user
                assert!(!rules.is_empty());
                write!(f, "case {expr} of")?;
                for rule in rules {
                    write!(f, "{NWLN}| {rule}")?;
                }
                write!(f, "{NWLN}end")
            }
        }
    }
}

impl<Ident: Display> Display for Pattern<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pattern::Var { var, .. } => {
                write!(f, "{var}")
            }
            Pattern::Lit { lit, .. } => {
                write!(f, "{lit}")
            }
            Pattern::Cons { cons, pars, .. } => {
                if pars.is_empty() {
                    write!(f, "{cons}")
                } else {
                    let pars = pars.iter().format(&", ");
                    write!(f, "{cons}({pars})")
                }
            }
            Pattern::Wild { .. } => {
                write!(f, "_")
            }
        }
    }
}

impl<Ident: Display> Display for Rule<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Rule { patn, body, .. } = self;
        if body.is_simple() {
            write!(f, "{patn} => {body}")
        } else {
            write!(f, "{patn} => {INDT}{NWLN}{body}{DEDT}{NWLN}")
        }
    }
}

impl<Ident: Display> Display for Varient<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Varient { cons, pars, .. } = self;
        if pars.is_empty() {
            write!(f, "{cons}")
        } else {
            let pars = pars.iter().format(&", ");
            write!(f, "{cons}[{pars}]")
        }
    }
}

impl<Ident: Display> Display for Decl<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Decl::Func {
                name, pars, body, ..
            } => {
                let pars = pars.iter().format(&", ");
                if body.is_simple() {
                    write!(f, "fun {name}({pars}) = {body};")
                } else {
                    write!(f, "fun {name}({pars}) ={INDT}{NWLN}{body}{DEDT}")
                }
            }
            Decl::Data {
                name, pars, vars, ..
            } => {
                if pars.is_empty() {
                    write!(f, "data {name} =")?;
                } else {
                    let pars = pars.iter().format(&", ");
                    write!(f, "data {name}[{pars}] =")?;
                }
                // Void can't be defined by user
                assert!(!vars.is_empty());
                for var in vars {
                    write!(f, "{NWLN}| {var}")?;
                }
                write!(f, "{NWLN}end")
            }
            Decl::Type {
                name, pars, typ, ..
            } => {
                if pars.is_empty() {
                    write!(f, "type {name} = {typ};")
                } else {
                    let pars = pars.iter().format(&", ");
                    write!(f, "type {name}[{pars}] = {typ};")
                }
            }
        }
    }
}

impl<Ident: Display> Display for MonoType<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MonoType::Lit(lit) => {
                write!(f, "{lit}")
            }
            MonoType::Var(var) => {
                write!(f, "{var}")
            }
            MonoType::Fun(args, res) => {
                let args = args.iter().format(&", ");
                write!(f, "fn ({args}) -> {res}")
            }
            MonoType::App(cons, args) => {
                assert!(!args.is_empty());
                let args = args.iter().format(&", ");
                write!(f, "{cons}[{args}]")
            }
        }
    }
}

#[test]
pub fn printer_ident_test() {
    let string1 = format!(
        "\n\
        hello{INDT}{NWLN}\
        world{INDT}{NWLN}\
        hello{INDT}{NWLN}\
        world{DEDT}{NWLN}\
        hello{DEDT}{NWLN}\
        world{DEDT}{NWLN}\
        hello world!\n\
    "
    );

    let string2 = r#"
hello
  world
    hello
      world
    hello
  world
hello world!
"#;

    assert_eq!(string1, string2)
}
