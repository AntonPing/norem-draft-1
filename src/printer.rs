use std::fmt::{self, Debug, Display};
use crate::ast::*;

pub struct INDT;
pub struct DEDT;
pub struct NWLN;

static mut INDT_LEVEL: usize = 0;

impl Display for INDT {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unsafe { INDT_LEVEL += 1; }
        Ok(())
    }
}

impl Display for DEDT {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        unsafe { INDT_LEVEL -= 1; }
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
        writeln!(f, "{:?}", self)
    }
}

impl Debug for DEDT {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{:?}", self)
    }
}

impl Debug for NWLN {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{:?}", self)
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

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Lit { lit, .. } => {
                write!(f, "{lit}")
            }
            Expr::Opr { op, args, .. } => {
                write!(f, "@{op}(")?;
                if !args.is_empty() {
                    write!(f, "{}", args[0])?;
                    for arg in &args[1..] {
                        write!(f, ",{arg}")?;
                    }
                }
                write!(f, ")")
            }
            Expr::Var { var, .. } => {
                write!(f, "{var}")
            }
            Expr::Fun { pars, body, .. } => {
                write!(f, "fn (")?;
                if !pars.is_empty() {
                    write!(f, "{}", pars[0])?;
                    for par in &pars[1..] {
                        write!(f, ",{par}")?;
                    }
                }
                write!(f, ") {{ {body} }}")
            }
            Expr::App { func, args, ..} => {
                write!(f, "{func}(")?;
                if !args.is_empty() {
                    write!(f, "{}", args[0])?;
                    for arg in &args[1..] {
                        write!(f, ",{arg}")?;
                    }
                }
                write!(f, ")")
            }
            Expr::Let { bind, expr, cont, .. } => {
                write!(f, "\
                    let {bind} = {expr};{NWLN}\
                    {cont}
                ")
            }
            Expr::Blk { decls, cont, .. } => {
                write!(f, "block{INDT}")?;
                for decl in decls {
                    write!(f, "{NWLN}{decl}")?;
                }
                write!(f, "{DEDT}{NWLN}\
                    in {INDT}{NWLN}\
                        {cont} {DEDT}{NWLN}\
                    end
                ")
            }
        }
    }
}

impl Display for Decl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Decl::Func { func, pars, expr, .. } => {
                write!(f, "fun {func}(")?;
                if !pars.is_empty() {
                    write!(f, "{}", pars[0])?;
                    for par in &pars[1..] {
                        write!(f, ",{par}")?;
                    }
                }
                write!(f, "){INDT}{NWLN}\
                    {expr};
                ")
            }
        }
    }
}


#[test]
pub fn printer_ident_test() {
    let string1 = format!("\
        hello{INDT}{NWLN}\
        world{INDT}{NWLN}\
        hello{INDT}{NWLN}\
        world{DEDT}{NWLN}\
        hello{DEDT}{NWLN}\
        world{DEDT}{NWLN}\
        hello world!\
    ");

    let string2 = 
r#"hello
  world
    hello
      world
    hello
  world
hello world!"#;

    assert_eq!(string1,string2)
}