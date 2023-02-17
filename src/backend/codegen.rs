use super::*;
use itertools::Itertools;
use std::collections::HashMap;
use std::fmt::{Result, Write};

pub struct Codegen {
    ext_map: HashMap<Ident, usize>,
    bind_vec: Vec<Ident>,
    is_main: bool,
    text: String,
}

impl Codegen {
    pub fn new(map: HashMap<Ident, usize>) -> Codegen {
        Codegen {
            ext_map: map,
            bind_vec: Vec::new(),
            is_main: false,
            text: String::new(),
        }
    }
    pub fn run(expr: &MExpr) -> String {
        let mut pass = Codegen::new(HashMap::new());
        pass.visit_toplevel(expr).unwrap();
        pass.text
    }

    fn visit_toplevel(&mut self, expr: &MExpr) -> Result {
        match expr {
            MExpr::LetIn { decls, cont } => {
                self.text.push_str(C_PROLOGUE);
                self.visit_extern_header()?;
                for decl in decls {
                    self.visit_decl_header(decl)?;
                }
                for decl in decls {
                    self.visit_decl(decl)?;
                }
                self.text.push_str("int main(int argc, char* argv[])\n{\n");
                self.text.push_str(C_SYS_CHECK);
                self.is_main = true;
                self.visit_expr(cont)?;
                self.text.push_str("}\n");
                self.text.push_str(C_EPILOGUE);
                Ok(())
            }
            _ => {
                panic!("after closure conversion there should be one and only one toplevel!");
            }
        }
    }

    fn visit_expr(&mut self, expr: &MExpr) -> Result {
        match expr {
            MExpr::LetIn { .. } => {
                panic!("after closure conversion there shouldn't be any nested let-block");
            }
            MExpr::Call {
                bind,
                func,
                args,
                cont,
            } => {
                // temp is used for pointer type coercing
                let temp = Ident::generate('f');
                let pars = args.iter().map(|_| "void*").format(", ");
                write!(self.text, "void* (*{temp})({pars}) = {func};\n")?;
                let args = args.iter().map(|arg| format!("(void*){arg}")).format(", ");
                write!(self.text, "void* {bind} = {temp}({args});\n")?;
                self.visit_expr(cont)
            }
            MExpr::ExtCall {
                bind,
                func,
                args,
                cont,
            } => {
                let args = args.iter().map(|arg| format!("(void*){arg}")).format(", ");
                write!(self.text, "void* {bind} = {func}({args});\n")?;
                self.visit_expr(cont)
            }
            MExpr::Retn { arg1 } => {
                match self.bind_vec.last() {
                    Some(ret_addr) => {
                        write!(self.text, "{ret_addr} = {arg1};\n")
                    }
                    None => {
                        // toplevel
                        if self.is_main {
                            write!(self.text, "return 0;\n")
                        } else {
                            write!(self.text, "return {arg1};\n")
                        }
                    }
                }
            }
            MExpr::UnOp {
                bind,
                prim,
                arg1,
                cont,
            } => {
                let (op, rhs) = match prim {
                    UnOpPrim::Move => ("", "void*"),
                    UnOpPrim::INeg => ("-", "int64_t"),
                };
                write!(self.text, "void* {bind} = (void*)({op}({rhs})({arg1}));\n")?;
                self.visit_expr(cont)
            }
            MExpr::BinOp {
                bind,
                prim,
                arg1,
                arg2,
                cont,
            } => {
                let (lhs, op, rhs) = match prim {
                    BinOpPrim::IAdd => ("int64_t", "+", "int64_t"),
                    BinOpPrim::ISub => ("int64_t", "-", "int64_t"),
                    BinOpPrim::IMul => ("int64_t", "*", "int64_t"),
                    BinOpPrim::ICmpGr => ("int64_t", ">", "int64_t"),
                    BinOpPrim::ICmpLs => ("int64_t", "<", "int64_t"),
                    BinOpPrim::ICmpEq => ("int64_t", "==", "int64_t"),
                    BinOpPrim::ICmpLe => ("int64_t", "<=", "int64_t"),
                    BinOpPrim::ICmpGe => ("int64_t", ">=", "int64_t"),
                    BinOpPrim::ICmpNe => ("int64_t", "!=", "int64_t"),
                };
                write!(
                    self.text,
                    "void* {bind} = (void*)(({lhs})({arg1}){op}({rhs})({arg2}));\n"
                )?;
                self.visit_expr(cont)
            }
            MExpr::Alloc { bind, size, cont } => {
                write!(
                    self.text,
                    "void* {bind} = malloc({size} * sizeof(void*));\n"
                )?;
                self.visit_expr(cont)
            }
            MExpr::Store {
                arg1,
                index,
                arg2,
                cont,
            } => {
                write!(self.text, "((void**){arg1})[{index}] = (void*)({arg2});\n")?;
                self.visit_expr(cont)
            }
            MExpr::Load {
                bind,
                arg1,
                index,
                cont,
            } => {
                write!(self.text, "void* {bind} = ((void**){arg1})[{index}];\n")?;
                self.visit_expr(cont)
            }
            MExpr::Offset {
                bind,
                arg1,
                index,
                cont,
            } => {
                write!(self.text, "void* {bind} = &((void**){arg1})[{index}];\n")?;
                self.visit_expr(cont)
            }
            MExpr::Ifte {
                bind,
                arg1,
                brch1,
                brch2,
                cont,
            } => {
                self.bind_vec.push(*bind);
                write!(self.text, "if({arg1})\n{{\n")?;
                self.visit_expr(brch1)?;
                write!(self.text, "}}\nelse\n{{\n")?;
                self.visit_expr(brch2)?;
                write!(self.text, "}}\n")?;
                self.bind_vec.pop();
                self.visit_expr(cont)
            }
            MExpr::Switch {
                bind,
                arg1,
                brchs,
                dflt,
                cont,
            } => {
                self.bind_vec.push(*bind);
                write!(self.text, "void* {bind};\n")?;
                write!(self.text, "switch((int64_t){arg1})\n{{\n")?;
                for (i, brch) in brchs.iter() {
                    write!(self.text, "case {i}:\n")?;
                    self.visit_expr(brch)?;
                    write!(self.text, "break;\n")?;
                }
                if let Some(dflt) = dflt {
                    write!(self.text, "default:\n")?;
                    self.visit_expr(dflt)?;
                }
                write!(self.text, "}}\n")?;
                self.bind_vec.pop();
                self.visit_expr(cont)
            }
        }
    }

    fn visit_extern_header(&mut self) -> Result {
        for (func, arity) in self.ext_map.iter() {
            let func = func.name;
            let pars = (0..*arity).map(|i| format!("void* arg{i}")).format(&", ");
            write!(self.text, "void* {func}({pars});\n")?;
        }
        Ok(())
    }

    fn visit_decl_header(&mut self, decl: &MDecl) -> Result {
        let MDecl { func, pars, .. } = decl;
        let pars = pars.iter().map(|par| format!("void* {par}")).format(&", ");
        write!(self.text, "void* {func}({pars});\n")
    }

    fn visit_decl(&mut self, decl: &MDecl) -> Result {
        let MDecl { func, pars, body } = decl;
        let pars = pars.iter().map(|par| format!("void* {par}")).format(&", ");
        write!(self.text, "void* {func}({pars})\n{{\n")?;
        assert!(self.bind_vec.is_empty());
        self.visit_expr(body)?;
        self.bind_vec.clear();
        write!(self.text, "}}\n")
    }
}

pub static C_PROLOGUE: &'static str = r#"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
"#;

pub static C_EPILOGUE: &'static str = r#"/*
this file is generated by norem compiler,
reading and editing are not recommanded.
*/
"#;

pub static C_SYS_CHECK: &'static str = r#"if(sizeof(void*) != 8)
{
puts("check failed: 'void*' is not 64-bits!");
exit(1);
}
if(sizeof(int64_t) != 8)
{
puts("check failed: 'int64_t' is not 64-bits!");
exit(1);
}
if(sizeof(double) != 8)
{
puts("check failed: 'double' is not 64-bits!");
exit(1);
}
"#;

#[test]
#[ignore]
fn codegen_test() {
    use super::anf_build::*;
    let expr1 = let_in(
        vec![fun(
            "f1",
            vec!["x1", "x2"],
            chain(vec![iadd("x3", v("x1"), v("x2")), retn(v("x3"))]),
        )],
        vec![call("x7", "x6", vec![v("x5")]), retn(v("x7"))],
    );
    let text1 = Codegen::run(&expr1);
    println!("{text1}");
    // todo: more tests
}
