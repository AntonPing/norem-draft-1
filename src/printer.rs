use crate::anf::*;
use crate::ast::*;
use itertools::{self, Itertools};
use std::cell::Cell;
use std::fmt::{self, Debug, Display};

pub struct INDT;
pub struct DEDT;
pub struct NWLN;

thread_local! {
    static INDT_LEVEL: Cell<usize> = Cell::new(0);
}

impl Display for INDT {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        INDT_LEVEL.with(|c| {
            let x = c.get();
            c.set(x + 1);
        });
        Ok(())
    }
}

impl Display for DEDT {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        INDT_LEVEL.with(|c| {
            let x = c.get();
            c.set(x - 1);
        });
        Ok(())
    }
}

impl Display for NWLN {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        INDT_LEVEL.with(|c| write!(f, "\n{:width$}", "", width = c.get() * 2))
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

impl<Ident: Display> Display for Type<Ident> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Lit { lit, .. } => {
                write!(f, "{lit}")
            }
            Type::Var { var, .. } => {
                write!(f, "{var}")
            }
            Type::Fun { pars, res, .. } => {
                let pars = pars.iter().format(&", ");
                write!(f, "fn ({pars}) -> {res}")
            }
            Type::App { cons, args, .. } => {
                assert!(!args.is_empty());
                let args = args.iter().format(&", ");
                write!(f, "{cons}[{args}]")
            }
        }
    }
}

impl Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Atom::Var(x) => write!(f, "{x}"),
            Atom::Int(x) => write!(f, "{x}"),
            Atom::Real(x) => write!(f, "{x}"),
            Atom::Bool(x) => write!(f, "{x}"),
            Atom::Char(x) => write!(f, "{x}"),
            Atom::Unit => write!(f, "()"),
        }
    }
}

impl Display for UnOpPrim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            UnOpPrim::Move => write!(f, "move"),
            UnOpPrim::INeg => write!(f, "ineg"),
        }
    }
}

impl Display for BinOpPrim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOpPrim::IAdd => write!(f, "iadd"),
            BinOpPrim::ISub => write!(f, "isub"),
            BinOpPrim::IMul => write!(f, "imul"),
        }
    }
}

impl Display for MExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MExpr::LetIn { decls, cont } => {
                write!(f, "letrec{INDT}")?;
                for decl in decls {
                    write!(f, "{NWLN}{decl}")?;
                }
                write!(f, "{DEDT}{NWLN}in{INDT}{NWLN}{cont}{DEDT}{NWLN}end")
            }
            MExpr::UnOp {
                bind,
                prim,
                arg1,
                cont,
            } => {
                write!(f, "let {bind} = {prim}({arg1});{NWLN}{cont}")
            }
            MExpr::BinOp {
                bind,
                prim,
                arg1,
                arg2,
                cont,
            } => {
                write!(f, "let {bind} = {prim}({arg1},{arg2});{NWLN}{cont}")
            }
            MExpr::Call {
                bind,
                func,
                args,
                cont,
            } => {
                let args = args.iter().format(&", ");
                write!(f, "let {bind} = {func}({args});{NWLN}{cont}")
            }
            MExpr::Retn { arg1 } => {
                write!(f, "return {arg1}")
            }
            MExpr::Alloc { bind, size, cont } => {
                write!(f, "let {bind} = alloc[{size}];{NWLN}{cont}")
            }
            MExpr::Load {
                bind,
                arg1,
                index,
                cont,
            } => {
                write!(f, "let {bind} = load {arg1}[{index}];{NWLN}{cont}")
            }
            MExpr::Store {
                arg1,
                index,
                arg2,
                cont,
            } => {
                write!(f, "store {arg1}[{index}] := {arg2};{NWLN}{cont}")
            }
            MExpr::Offset {
                bind,
                arg1,
                index,
                cont,
            } => {
                write!(f, "let {bind} = offset {arg1}[{index}];{NWLN}{cont}")
            }
            MExpr::Ifte {
                bind,
                arg1,
                brch1,
                brch2,
                cont,
            } => {
                write!(f, "let {bind} = if({arg1}) then")?;
                write!(f, "{INDT}{NWLN}{brch1}{DEDT}{NWLN}else")?;
                write!(f, "{INDT}{NWLN}{brch2}{DEDT}{NWLN};{NWLN}{cont}")
            }
            MExpr::Switch {
                bind,
                arg1,
                brchs,
                cont,
            } => {
                write!(f, "let {bind} = switch({arg1}) {{{INDT}")?;
                for (i, brch) in brchs.iter().enumerate() {
                    write!(f, "{NWLN}case {i}:{INDT}{NWLN}{brch}{DEDT}")?;
                }
                write!(f, "{DEDT}{NWLN}}}{NWLN}{cont}")
            }
        }
    }
}

impl Display for MDecl {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MDecl { func, pars, body } = self;
        let pars = pars.iter().format(&", ");
        write!(f, "fun {func}({pars}) = {INDT}{NWLN}{body}{DEDT}")
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
