use itertools::Itertools;
use std::fmt::Result;
use std::fmt::Write;

use crate::anf::*;
use crate::intern::Ident;

pub struct Codegen {
    bind_vec: Vec<Ident>,
    text: String,
}

impl Codegen {
    pub fn new() -> Codegen {
        Codegen {
            bind_vec: Vec::new(),
            text: String::new(),
        }
    }
    pub fn run(expr: &MExpr) -> String {
        let mut pass = Codegen::new();
        pass.visit_toplevel(expr).unwrap();
        pass.text
    }

    fn visit_toplevel(&mut self, expr: &MExpr) -> Result {
        match expr {
            MExpr::LetIn { decls, cont } => {
                self.text.push_str(C_PROLOGUE);
                for decl in decls {
                    self.visit_decl_header(decl)?;
                }
                for decl in decls {
                    self.visit_decl(decl)?;
                }
                self.text.push_str("int main(int argc, char* argv[])\n{\n");
                self.text.push_str(C_SYS_CHECK);
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
                        write!(self.text, "return {arg1};\n")
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
fn dump_c_code() {
    use crate::clos_conv::ClosConv;
    use crate::normalize::Normalize;
    use crate::parser::{parse_expr, Parser};
    use crate::renamer::Renamer;
    use crate::simple_opt::LinearInline;
    use crate::simple_opt::{ConstFold, DeadElim};
    use std::fs::{self, File};
    use std::io::Write;
    let string = r#"
begin
    data List[T] =
    | Cons(T,List[T])
    | Nil
    end
    fun length(lst) => {
        case lst of
        | Cons(head,tail) => {
            @iadd(length(tail),1)
        }
        | Nil => { 0 }
        end
    }
in
    length(Cons(1,Cons(2,Cons(3,Cons(4,Nil)))))
end
"#;

    let mut par = Parser::new(string);
    let expr1 = parse_expr(&mut par).unwrap();
    let mut rnm = Renamer::new();
    let expr1 = rnm.visit_expr(expr1);
    let expr1 = Normalize::run(&expr1);
    println!("normalize:\n{expr1}");
    let expr1 = DeadElim::run(expr1);
    println!("dead-elim:\n{expr1}");
    let expr1 = ConstFold::run(expr1);
    println!("const-fold:\n{expr1}");
    let expr1 = LinearInline::run(expr1);
    println!("linear-inline:\n{expr1}");
    let expr1 = ClosConv::run(expr1);
    println!("clos-conv\n{expr1}");
    let expr1 = DeadElim::run(expr1);
    println!("dead-elim:\n{expr1}");
    let expr1 = ConstFold::run(expr1);
    println!("const-fold:\n{expr1}");
    let expr1 = LinearInline::run(expr1);
    println!("linear-inline:\n{expr1}");
    let text = Codegen::run(&expr1);
    fs::create_dir_all("./target/output").unwrap();
    let mut output = File::create("./target/output/test_output.c").unwrap();
    output.write(text.as_bytes()).unwrap();
}

#[test]
#[ignore]
fn codegen_test() {
    use crate::anf::anf_build::*;
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
