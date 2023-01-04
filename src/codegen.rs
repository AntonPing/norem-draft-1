use itertools::Itertools;
use std::fmt::Result;
use std::fmt::Write;

use crate::anf::*;
use crate::intern::Unique;

pub struct Codegen {
    text: String,
}

impl Codegen {
    pub fn new() -> Codegen {
        Codegen {
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
                self.text.push_str("int main(int argc, char* argv[]) {\n");
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
    fn binop(
        &mut self,
        bind: &Option<Unique>,
        args: &Vec<Atom>,
        lhs: &str,
        opr: &str,
        rhs: &str,
    ) -> Result {
        if let [arg1, arg2] = &args[..] {
            if let Some(x) = bind {
                write!(
                    self.text,
                    "  void* {x} = (void*)(({lhs}){arg1} {opr} ({rhs}){arg2});\n"
                )
            } else {
                write!(self.text, "  (void*)(({lhs}){arg1} {opr} ({rhs}){arg2});\n")
            }
        } else {
            panic!("argument number doesn't match operator's arity!");
        }
    }

    fn uniop(&mut self, bind: &Option<Unique>, args: &Vec<Atom>, opr: &str, rhs: &str) -> Result {
        if let [arg1] = &args[..] {
            if let Some(x) = bind {
                write!(self.text, "  void* {x} = (void*)({opr} ({rhs}){arg1});\n")
            } else {
                write!(self.text, "  (void*)({opr} ({rhs}){arg1});\n")
            }
        } else {
            panic!("argument number doesn't match operator's arity!");
        }
    }

    fn jump_compare(
        &mut self,
        args: &Vec<Atom>,
        conts: &Vec<MExpr>,
        lhs: &str,
        opr: &str,
        rhs: &str,
    ) -> Result {
        if let ([arg1, arg2], [trbr, flbr]) = (&args[..], &conts[..]) {
            write!(self.text, "  if({lhs}){arg1} {opr} ({rhs}){arg2}) {{\n")?;
            self.visit_expr(trbr)?;
            write!(self.text, "  }} else {{\n")?;
            self.visit_expr(flbr)?;
            write!(self.text, "  }}\n")
        } else {
            panic!("argument number doesn't match operator's arity!");
        }
    }

    fn visit_expr(&mut self, expr: &MExpr) -> Result {
        match expr {
            MExpr::LetIn { .. } => {
                panic!("after closure conversion there shouldn't be any nested let-block");
            }
            MExpr::Stmt {
                bind,
                prim,
                args,
                cont,
            } => {
                match prim {
                    StmtPrim::IAdd => {
                        self.binop(bind, args, "int64_t", "+", "int64_t")?;
                    }
                    StmtPrim::ISub => {
                        self.binop(bind, args, "int64_t", "-", "int64_t")?;
                    }
                    StmtPrim::IMul => {
                        self.binop(bind, args, "int64_t", "*", "int64_t")?;
                    }
                    StmtPrim::IDiv => {
                        self.binop(bind, args, "int64_t", "/", "int64_t")?;
                    }
                    StmtPrim::IRem => {
                        self.binop(bind, args, "int64_t", "%", "int64_t")?;
                    }
                    StmtPrim::INeg => {
                        self.uniop(bind, args, "-", "int64_t")?;
                    }
                    StmtPrim::RAdd => {
                        self.binop(bind, args, "double", "+", "double")?;
                    }
                    StmtPrim::RSub => {
                        self.binop(bind, args, "double", "-", "double")?;
                    }
                    StmtPrim::RMul => {
                        self.binop(bind, args, "double", "*", "double")?;
                    }
                    StmtPrim::RDiv => {
                        self.binop(bind, args, "double", "/", "double")?;
                    }
                    StmtPrim::BAnd => {
                        self.binop(bind, args, "bool", "&&", "bool")?;
                    }
                    StmtPrim::BOr => {
                        self.binop(bind, args, "bool", "||", "bool")?;
                    }
                    StmtPrim::BNot => {
                        self.uniop(bind, args, "!", "bool")?;
                    }
                    StmtPrim::Move => {
                        self.uniop(bind, args, "", "void*")?;
                    }
                    StmtPrim::Alloc => {
                        if let [arg1] = &args[..] {
                            if let Some(x) = bind {
                                write!(
                                    self.text,
                                    "  void* {x} = malloc({arg1} * sizeof(void*));\n"
                                )?;
                            } else {
                                write!(self.text, "  malloc({arg1} * sizeof(void*));\n")?;
                            }
                        } else {
                            panic!("argument number doesn't match operator's arity!");
                        }
                    }
                    StmtPrim::Load => {
                        if let [arg1, arg2] = &args[..] {
                            if let Some(x) = bind {
                                write!(self.text, "  void* {x} = ((void**){arg1})[{arg2}];\n")?;
                            } else {
                                write!(self.text, "  ((void**){arg1})[{arg2}];\n")?;
                            }
                        } else {
                            panic!("argument number doesn't match operator's arity!");
                        }
                    }
                    StmtPrim::Store => {
                        if let [arg1, arg2, arg3] = &args[..] {
                            assert!(bind.is_none());
                            write!(self.text, "  ((void**){arg1})[{arg2}] = (void*){arg3};\n")?;
                        } else {
                            panic!("argument number doesn't match operator's arity!");
                        }
                    }
                    StmtPrim::Offset => {
                        if let [arg1, arg2] = &args[..] {
                            if let Some(x) = bind {
                                write!(self.text, "  void* {x} = &((void**){arg1})[{arg2}];\n")?;
                            } else {
                                write!(self.text, "  &((void**){arg1})[{arg2}];\n")?;
                            }
                        } else {
                            panic!("argument number doesn't match operator's arity!");
                        }
                    }
                }
                self.visit_expr(cont)
            }
            MExpr::Brch { prim, args, conts } => {
                match prim {
                    BrchPrim::Switch => {
                        if let [arg1] = &args[..] {
                            write!(self.text, "  switch({arg1}) {{\n")?;
                            for (i, cont) in conts.iter().enumerate() {
                                write!(self.text, "  case {i}:\n")?;
                                self.visit_expr(cont)?;
                                write!(self.text, "  break;\n")?;
                            }
                            write!(self.text, "  }}\n")
                        } else {
                            panic!("expected 1 arguments, found {}!", args.len());
                        }
                    }
                    BrchPrim::Ifte => {
                        if let ([arg1], [trbr, flbr]) = (&args[..], &conts[..]) {
                            write!(self.text, "  if({arg1}) {{\n")?;
                            self.visit_expr(trbr)?;
                            write!(self.text, "  }} else {{\n")?;
                            self.visit_expr(flbr)?;
                            write!(self.text, "  }}\n")
                        } else {
                            panic!("expected 1 argument and 2 branches, found {} and {} correspondingly!", args.len(), conts.len());
                        }
                    }
                    BrchPrim::JumpGr => self.jump_compare(args, conts, "int64_t", ">", "int64_t"),
                    BrchPrim::JumpLs => self.jump_compare(args, conts, "int64_t", "<", "int64_t"),
                    BrchPrim::JumpEq => self.jump_compare(args, conts, "int64_t", "==", "int64_t"),
                    // todo: not only integer compare, but also real number compare
                    BrchPrim::Halt => {
                        write!(self.text, "exit(0)\n")
                    }
                }
            }
            MExpr::Call {
                bind,
                func,
                args,
                cont,
            } => {
                // temp is used for pointer type coercing
                let temp = Unique::generate('f');
                let pars = args.iter().map(|_| "void*").format(", ");
                write!(self.text, "  void* (*{temp})({pars}) = {func};\n")?;
                let args = args.iter().map(|arg| format!("(void*){arg}")).format(", ");
                if let Some(x) = bind {
                    write!(self.text, "  void* {x} = {temp}({args});\n")?;
                } else {
                    write!(self.text, "  {temp}({args});\n")?;
                }
                self.visit_expr(cont)
            }
            MExpr::Retn { atom } => {
                write!(self.text, "  return {atom};\n")
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
        write!(self.text, "void* {func}({pars}) {{\n")?;
        self.visit_expr(body)?;
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

pub static C_SYS_CHECK: &'static str = r#"  if(sizeof(void*) != 8) {
    puts("check failed: 'void*' is not 64-bits!");
    exit(1);
  }
  if(sizeof(int64_t) != 8) {
    puts("check failed: 'int64_t' is not 64-bits!");
    exit(1);
  }
  if(sizeof(double) != 8) {
    puts("check failed: 'double' is not 64-bits!");
    exit(1);
  }
"#;

#[test]
#[ignore]
fn dump_c_code() {
    use crate::clos_conv::ClosConv;
    use crate::normalize::normalize_expr;
    use crate::parser::{parse_expr, Parser};
    use crate::renamer::Renamer;
    use std::fs::{self, File};
    use std::io::Write;

    let string = r#"
let f = fun(x) => fun(y) => @iadd(x,y);
f(1)(2)
    "#;
    let mut par = Parser::new(string);
    let expr1 = parse_expr(&mut par).unwrap();
    let mut rnm = Renamer::new();
    let expr1 = rnm.visit_expr(expr1);
    let expr1 = normalize_expr(&expr1);
    let expr1 = ClosConv::run(expr1);
    let text = Codegen::run(&expr1);
    fs::create_dir_all("./target/output").unwrap();
    let mut output = File::create("./target/output/test_output.c").unwrap();
    output.write(text.as_bytes()).unwrap();
}

#[test]
#[ignore]
#[allow(unused_imports)]
fn codegen_test() {
    use crate::anf::BrchPrim::*;
    use crate::anf::StmtPrim::*;
    use crate::parser::*;
    use crate::renamer::Renamer;
    use crate::{atom, decls, expr, ident};

    let expr1 = expr! {
        letin [
            fun f1 (x1,x2) => {
                stmt x3 = IAdd, x1, x2;
                retn x3;
            }
        ] {
            call x7 = x6 (x5);
            retn x7;
        }
    };
    let text1 = Codegen::run(&expr1);
    println!("{text1}");

    // todo: more tests
}
