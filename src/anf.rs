use crate::ast::{Builtin, LitVal};
use crate::intern::{InternStr, Unique};
use std::collections::HashMap;
use std::fmt::{self, Display};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum StmtPrim {
    // arithmetic operations
    IAdd,
    ISub,
    IMul,
    IDiv,
    IRem,
    INeg,
    RAdd,
    RSub,
    RMul,
    RDiv,
    BAnd,
    BOr,
    BNot,
    // transfer operation
    Move,
    // memory operations
    Alloc,
    Load,
    Store,
    Offset,
}

impl StmtPrim {
    pub fn is_pure(&self) -> bool {
        use StmtPrim::*;
        match self {
            IAdd | ISub | IMul | IDiv | IRem | INeg | RAdd | RSub | RMul | RDiv | BAnd | BOr
            | BNot | Move | Offset => true,
            _ => false,
        }
    }
    pub fn arity(&self) -> usize {
        use StmtPrim::*;
        match self {
            IAdd | ISub | IMul | IDiv | IRem => 2,
            INeg => 1,
            RAdd | RSub | RMul | RDiv => 2,
            BAnd | BOr => 2,
            BNot => 1,
            Move => 1,
            Alloc => 1,
            Load => 2,
            Store => 3,
            Offset => 2,
        }
    }
}

impl From<Builtin> for StmtPrim {
    fn from(prim: Builtin) -> Self {
        match prim {
            Builtin::IAdd => StmtPrim::IAdd,
            Builtin::ISub => StmtPrim::ISub,
            Builtin::IMul => StmtPrim::IMul,
            Builtin::IDiv => StmtPrim::IDiv,
            Builtin::IRem => StmtPrim::IRem,
            Builtin::INeg => StmtPrim::INeg,
            Builtin::RAdd => StmtPrim::RAdd,
            Builtin::RSub => StmtPrim::RSub,
            Builtin::RMul => StmtPrim::RMul,
            Builtin::RDiv => StmtPrim::RDiv,
            Builtin::BAnd => StmtPrim::BAnd,
            Builtin::BOr => StmtPrim::BOr,
            Builtin::BNot => StmtPrim::BNot,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BrchPrim {
    // switch branching
    Switch,
    // if-then-else branching
    Ifte,
    // compare-jump branching
    JumpGr,
    JumpLs,
    JumpEq,
    // Termination
    Halt,
}

impl BrchPrim {
    pub fn arity(&self) -> usize {
        match self {
            BrchPrim::Switch => 1,
            BrchPrim::Ifte => 1,
            BrchPrim::JumpGr => 2,
            BrchPrim::JumpLs => 2,
            BrchPrim::JumpEq => 2,
            BrchPrim::Halt => 0,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Atom {
    Var(Unique),
    Int(i64),
    Real(f64),
    Bool(bool),
    Char(char),
}

impl From<LitVal> for Atom {
    fn from(lit: LitVal) -> Self {
        match lit {
            LitVal::Int(x) => Atom::Int(x),
            LitVal::Real(x) => Atom::Real(x),
            LitVal::Bool(x) => Atom::Bool(x),
            LitVal::Char(x) => Atom::Char(x),
        }
    }
}

impl From<Unique> for Atom {
    fn from(var: Unique) -> Self {
        Atom::Var(var)
    }
}

impl From<i64> for Atom {
    fn from(x: i64) -> Self {
        Atom::Int(x)
    }
}

impl From<f64> for Atom {
    fn from(x: f64) -> Self {
        Atom::Real(x)
    }
}

impl From<bool> for Atom {
    fn from(x: bool) -> Self {
        Atom::Bool(x)
    }
}

impl From<char> for Atom {
    fn from(x: char) -> Self {
        Atom::Char(x)
    }
}

impl Atom {
    pub fn is_literal(&self) -> bool {
        match self {
            Atom::Int(_) => true,
            Atom::Real(_) => true,
            Atom::Bool(_) => true,
            Atom::Char(_) => true,
            _ => false,
        }
    }
    pub fn is_variable(&self) -> bool {
        match self {
            Atom::Var(_) => true,
            _ => false,
        }
    }
    pub fn unwrap_literal(self) -> LitVal {
        match self {
            Atom::Int(x) => LitVal::Int(x),
            Atom::Real(x) => LitVal::Real(x),
            Atom::Bool(x) => LitVal::Bool(x),
            Atom::Char(x) => LitVal::Char(x),
            _ => panic!("failed to unwrap literal!"),
        }
    }
    pub fn unwrap_variable(self) -> Unique {
        match self {
            Atom::Var(x) => x,
            _ => panic!("failed to unwrap variable!"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum CallFunc {
    Intern(Unique),
    Extern(InternStr),
}

#[derive(Clone, Debug)]
pub enum MExpr {
    LetIn {
        decls: Vec<MDecl>,
        cont: Box<MExpr>,
    },
    Stmt {
        bind: Option<Unique>,
        prim: StmtPrim,
        args: Vec<Atom>,
        cont: Box<MExpr>,
    },
    Brch {
        prim: BrchPrim,
        args: Vec<Atom>,
        conts: Vec<MExpr>,
    },
    Call {
        bind: Option<Unique>,
        func: CallFunc,
        args: Vec<Atom>,
        cont: Box<MExpr>,
    },
    Retn {
        atom: Atom,
    },
}

impl MExpr {
    pub fn is_tail_call(&self) -> bool {
        if let MExpr::Call { cont, .. } = self {
            if let MExpr::Retn { .. } = **cont {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn make_tail_call(func: Unique, args: Vec<Atom>) -> MExpr {
        let r = Unique::generate('r');
        MExpr::Call {
            bind: Some(r),
            func: CallFunc::Intern(func),
            args,
            cont: Box::new(MExpr::Retn { atom: Atom::Var(r) }),
        }
    }
}

#[derive(Clone, Debug)]
pub struct MDecl {
    pub func: Unique,
    pub pars: Vec<Unique>,
    pub body: MExpr,
}

impl Display for StmtPrim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StmtPrim::IAdd => write!(f, "iadd"),
            StmtPrim::ISub => write!(f, "isub"),
            StmtPrim::IMul => write!(f, "imul"),
            StmtPrim::IDiv => write!(f, "idiv"),
            StmtPrim::IRem => write!(f, "irem"),
            StmtPrim::INeg => write!(f, "ineg"),
            StmtPrim::RAdd => write!(f, "radd"),
            StmtPrim::RSub => write!(f, "rsub"),
            StmtPrim::RMul => write!(f, "rmul"),
            StmtPrim::RDiv => write!(f, "rdiv"),
            StmtPrim::BAnd => write!(f, "band"),
            StmtPrim::BOr => write!(f, "bor"),
            StmtPrim::BNot => write!(f, "bnot"),
            StmtPrim::Move => write!(f, "move"),
            StmtPrim::Alloc => write!(f, "alloc"),
            StmtPrim::Load => write!(f, "load"),
            StmtPrim::Store => write!(f, "store"),
            StmtPrim::Offset => write!(f, "offset"),
        }
    }
}

impl Display for BrchPrim {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BrchPrim::Switch => write!(f, "switch"),
            BrchPrim::Ifte => write!(f, "ifte"),
            BrchPrim::JumpGr => write!(f, "jumpgr"),
            BrchPrim::JumpLs => write!(f, "jumpls"),
            BrchPrim::JumpEq => write!(f, "jumpeq"),
            BrchPrim::Halt => write!(f, "halt"),
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
        }
    }
}

impl Display for CallFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            CallFunc::Intern(func) => write!(f, "{func}"),
            CallFunc::Extern(func) => write!(f, "{func}"),
        }
    }
}

struct AlphaEquiv {
    map: HashMap<Unique, Unique>,
}

impl AlphaEquiv {
    fn new() -> AlphaEquiv {
        AlphaEquiv {
            map: HashMap::new(),
        }
    }

    fn eq_ident(&mut self, ident1: &Unique, ident2: &Unique) -> Option<()> {
        // println!("unify {ident1} and {ident2}");
        if let Some(bind) = self.map.get(&ident1) {
            if bind == ident2 {
                Some(())
            } else {
                None
            }
        } else {
            self.map.insert(*ident1, *ident2);
            Some(())
        }
    }

    fn eq_atom(&mut self, atom1: &Atom, atom2: &Atom) -> Option<()> {
        match (atom1, atom2) {
            (Atom::Var(var1), Atom::Var(var2)) => self.eq_ident(var1, var2),
            (atom1, atom2) => {
                if atom1 == atom2 {
                    Some(())
                } else {
                    None
                }
            }
        }
    }

    fn eq_bind(&mut self, bind1: &Option<Unique>, bind2: &Option<Unique>) -> Option<()> {
        match (bind1, bind2) {
            (Some(bind1), Some(bind2)) => self.eq_ident(bind1, bind2),
            (Some(_), None) | (None, Some(_)) => None,
            (None, None) => Some(()),
        }
    }

    fn eq_call_func(&mut self, func1: &CallFunc, func2: &CallFunc) -> Option<()> {
        match (func1, func2) {
            (CallFunc::Intern(ident1), CallFunc::Intern(ident2)) => self.eq_ident(ident1, ident2),
            (CallFunc::Intern(_), CallFunc::Extern(_))
            | (CallFunc::Extern(_), CallFunc::Intern(_)) => None,
            (CallFunc::Extern(s1), CallFunc::Extern(s2)) => {
                if s1 == s2 {
                    Some(())
                } else {
                    None
                }
            }
        }
    }

    fn eq_decl(&mut self, decl1: &MDecl, decl2: &MDecl) -> Option<()> {
        let MDecl {
            func: func1,
            pars: pars1,
            body: body1,
        } = decl1;
        let MDecl {
            func: func2,
            pars: pars2,
            body: body2,
        } = decl2;

        self.eq_ident(func1, func2)?;
        if pars1.len() != pars2.len() {
            return None;
        }
        for (par1, par2) in pars1.iter().zip(pars2.iter()) {
            self.eq_ident(par1, par2)?;
        }
        self.eq_expr(body1, body2)
    }

    fn eq_expr(&mut self, expr1: &MExpr, expr2: &MExpr) -> Option<()> {
        match (expr1, expr2) {
            (
                MExpr::LetIn {
                    decls: decls1,
                    cont: cont1,
                },
                MExpr::LetIn {
                    decls: decls2,
                    cont: cont2,
                },
            ) => {
                if decls1.len() != decls2.len() {
                    return None;
                }
                for (decl1, decl2) in decls1.iter().zip(decls2.iter()) {
                    self.eq_decl(decl1, decl2)?;
                }
                self.eq_expr(cont1, cont2)
            }
            (
                MExpr::Stmt {
                    bind: bind1,
                    prim: prim1,
                    args: args1,
                    cont: cont1,
                },
                MExpr::Stmt {
                    bind: bind2,
                    prim: prim2,
                    args: args2,
                    cont: cont2,
                },
            ) => {
                self.eq_bind(bind1, bind2)?;
                if prim1 != prim2 {
                    return None;
                }
                if args1.len() != args2.len() {
                    return None;
                }
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    self.eq_atom(arg1, arg2)?;
                }
                self.eq_expr(cont1, cont2)
            }
            (
                MExpr::Brch {
                    prim: prim1,
                    args: args1,
                    conts: conts1,
                },
                MExpr::Brch {
                    prim: prim2,
                    args: args2,
                    conts: conts2,
                },
            ) => {
                if prim1 != prim2 {
                    return None;
                }
                if args1.len() != args2.len() {
                    return None;
                }
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    self.eq_atom(arg1, arg2)?;
                }
                if conts1.len() != conts2.len() {
                    return None;
                }
                for (cont1, cont2) in conts1.iter().zip(conts2.iter()) {
                    self.eq_expr(cont1, cont2)?;
                }
                Some(())
            }
            (
                MExpr::Call {
                    bind: bind1,
                    func: func1,
                    args: args1,
                    cont: cont1,
                },
                MExpr::Call {
                    bind: bind2,
                    func: func2,
                    args: args2,
                    cont: cont2,
                },
            ) => {
                self.eq_bind(bind1, bind2)?;
                self.eq_call_func(func1, func2)?;
                if args1.len() != args2.len() {
                    return None;
                }
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    self.eq_atom(arg1, arg2)?;
                }
                self.eq_expr(cont1, cont2)
            }

            (MExpr::Retn { atom: atom1 }, MExpr::Retn { atom: atom2 }) => {
                self.eq_atom(atom1, atom2)
            }
            (_, _) => None,
        }
    }
}

impl PartialEq for MExpr {
    fn eq(&self, other: &Self) -> bool {
        // use alpha-equivalence instead of identical comparison
        let mut pass = AlphaEquiv::new();
        pass.eq_expr(self, other).is_some()
    }
}

impl PartialEq for MDecl {
    fn eq(&self, other: &Self) -> bool {
        // use alpha-equivalence instead of identical comparison
        let mut pass = AlphaEquiv::new();
        pass.eq_decl(self, other).is_some()
    }
}

#[macro_export]
macro_rules! ident {
    ($var:ident) => {
        crate::intern::intern(stringify!($var)).as_dummy()
    };
}

#[macro_export]
macro_rules! atom {
    ($lit:literal) => {
        $lit.into()
    };
    ($var:ident) => {
        crate::intern::intern(stringify!($var)).as_dummy().into()
    };
}

#[macro_export]
macro_rules! decls {
    ($(fun $func:ident ( $($args:ident),* ) => { $($body:tt)+ });+) => {
        vec![$(
            MDecl {
                func: ident!($func),
                pars: vec![$(ident!($args),)*],
                body: expr!($($body)+),
            },
        )+]
    };
}

#[macro_export]
macro_rules! expr {
    (retn $atom:tt;) => {
        MExpr::Retn {
            atom: atom!($atom)
        }
    };
    (stmt $bind:ident = $prim:expr, $($args:tt),+ ; $($rest:tt)*) => {
        {
            let args_vec = vec![$(atom!($args),)*];
            assert_eq!(args_vec.len(), $prim.arity());
            MExpr::Stmt {
                bind: Some(ident!($bind)),
                prim: $prim,
                args: args_vec,
                cont: Box::new(expr!($($rest)*)),
            }
        }
    };
    (stmt $prim:expr, $($args:tt),+ ; $($rest:tt)*) => {
        {
            let args_vec = vec![$(atom!($args),)*];
            assert_eq!(args_vec.len(), $prim.arity());
            MExpr::Stmt {
                bind: None,
                prim: $prim,
                args: args_vec,
                cont: Box::new(expr!($($rest)*)),
            }
        }
    };
    (brch $prim:expr, $($args:tt),+ begin $({ $($rest:tt)* });* end) => {
        {
            let args_vec = vec![$(atom!($args),)*];
            assert_eq!(args_vec.len(), $prim.arity());
            MExpr::Brch {
                prim: $prim,
                args: args_vec,
                conts: vec![$(expr!($($rest)*)),*]
            }
        }
    };
    (call $bind:ident = $func:ident ( $($args:tt),+ ) ; $($rest:tt)*) => {
        {
            let args_vec = vec![$(atom!($args),)*];
            MExpr::Call {
                bind: Some(ident!($bind)),
                func: CallFunc::Intern(ident!($func)),
                args: args_vec,
                cont: Box::new(expr!($($rest)*)),
            }
        }
    };
    (call $func:ident ( $($args:tt),+ ) ; $($rest:tt)*) => {
        {
            let args_vec = vec![$(atom!($args),)*];
            MExpr::Call {
                bind: None,
                func: CallFunc::Intern(ident!($func)),
                args: args_vec,
                cont: Box::new(expr!($($rest)*)),
            }
        }
    };
    (letin [ $($decls:tt)* ] { $($rest:tt)* } ) => {
        MExpr::LetIn {
            decls: decls!($($decls)*),
            cont: Box::new(expr!($($rest)*)),
        }
    }
}

#[test]
#[ignore]
fn anf_macro_test() {
    use BrchPrim::*;
    use StmtPrim::*;

    let expr = expr! {
        letin [
            fun f (x, y) => {
                brch JumpGr, x, y begin {
                    retn x;
                }; {
                    retn y;
                } end
            }
        ] {
            stmt a = IAdd, 1, 2;
            stmt b = IMul, 3, 4;
            call res = f (a, b);
            retn res;
        }
    };
    println!("{expr}")
}

#[test]
fn alpha_equiv_test() {
    use StmtPrim::*;

    let expr1 = expr! {
        stmt x = IAdd, 1, 2;
        stmt y = IMul, x, x;
        retn y;
    };

    let expr2 = expr! {
        stmt a = IAdd, 1, 2;
        stmt b = IMul, a, a;
        retn b;
    };

    let expr3 = expr! {
        stmt a = IAdd, 1, 2;
        stmt b = IMul, a, a;
        retn a;
    };

    assert_eq!(expr1, expr2);
    assert_ne!(expr1, expr3);
    assert_ne!(expr2, expr3);
}
