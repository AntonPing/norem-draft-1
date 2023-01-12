use itertools::Itertools;
use std::fmt::Result;
use std::fmt::Write;

use crate::anf::*;
use crate::intern::Unique;

pub struct Codegen {
    bind_vec: Vec<Option<Unique>>,
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

    fn visit_expr(&mut self, expr: &MExpr) -> Result {
        match expr {
            MExpr::LetIn { .. } => {
                panic!("after closure conversion there shouldn't be any nested let-block");
            }
            MExpr::Stmt { bind, stmt, cont } if stmt.is_branch() => {
                if let Some(ret_addr) = bind {
                    write!(self.text, "    void* {ret_addr};\n")?;
                }
                self.bind_vec.push(*bind);
                match stmt {
                    MStmt::Ifte { arg1, brch1, brch2 } => {
                        write!(self.text, "    if({arg1}) {{\n")?;
                        self.visit_expr(brch1)?;
                        write!(self.text, "    }} else {{\n")?;
                        self.visit_expr(brch2)?;
                        write!(self.text, "    }}\n")?;
                    }
                    MStmt::Switch { arg1, brchs } => {
                        write!(self.text, "    switch({arg1}) {{\n")?;
                        for (i, brch) in brchs.iter().enumerate() {
                            write!(self.text, "    case {i}:\n")?;
                            self.visit_expr(brch)?;
                            write!(self.text, "    break;\n")?;
                        }
                        write!(self.text, "    }}\n")?;
                    }
                    _ => {
                        unreachable!()
                    }
                }
                self.bind_vec.pop();
                self.visit_expr(cont)
            }
            MExpr::Stmt { bind, stmt, cont } => {
                if let Some(x) = bind {
                    write!(self.text, "    void* {x} = (void*)(")?;
                } else {
                    write!(self.text, "    ")?;
                }

                match stmt {
                    MStmt::IAdd { arg1, arg2 } => {
                        write!(self.text, "(int64_t){arg1} + (int64_t){arg2}")?
                    }
                    MStmt::ISub { arg1, arg2 } => {
                        write!(self.text, "(int64_t){arg1} - (int64_t){arg2}")?
                    }
                    MStmt::IMul { arg1, arg2 } => {
                        write!(self.text, "(int64_t){arg1} * (int64_t){arg2}")?
                    }
                    MStmt::Move { arg1 } => write!(self.text, "{arg1}")?,
                    MStmt::Alloc { size } => write!(self.text, "malloc({size} * sizeof(void*))")?,
                    MStmt::Load { arg1, index } => write!(self.text, "((void**){arg1})[{index}]")?,
                    MStmt::Store { arg1, index, arg2 } => {
                        assert!(bind.is_none());
                        write!(self.text, "((void**){arg1})[{index}] = (void*){arg2}")?
                    }
                    MStmt::Offset { arg1, index } => {
                        write!(self.text, "&((void**){arg1})[{index}]")?
                    }
                    MStmt::Ifte { .. } | MStmt::Switch { .. } => {
                        unreachable!()
                    }
                }

                if bind.is_some() {
                    write!(self.text, ");\n")?;
                } else {
                    write!(self.text, ";\n")?;
                }
                self.visit_expr(cont)
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
                write!(self.text, "    void* (*{temp})({pars}) = {func};\n")?;
                let args = args.iter().map(|arg| format!("(void*){arg}")).format(", ");
                if let Some(x) = bind {
                    write!(self.text, "    void* {x} = {temp}({args});\n")?;
                } else {
                    write!(self.text, "    {temp}({args});\n")?;
                }
                self.visit_expr(cont)
            }
            MExpr::Retn { atom } => {
                match self.bind_vec.last() {
                    Some(Some(ret_addr)) => {
                        write!(self.text, "    {ret_addr} = {atom};\n")
                    }
                    Some(None) => Ok(()),
                    None => {
                        // toplevel
                        write!(self.text, "    return {atom};\n")
                    }
                }
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

pub static C_SYS_CHECK: &'static str = r#"
    if(sizeof(void*) != 8) {
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
fn codegen_test() {
    use crate::anf::anf_build::*;
    let expr1 = letin_block(
        vec![fun(
            "f1",
            vec!["x1", "x2"],
            block(vec![iadd("x3", v("x1"), v("x2")), retn(v("x3"))]),
        )],
        vec![call("x7", "x6", vec![v("x5")]), retn(v("x7"))],
    );
    let text1 = Codegen::run(&expr1);
    println!("{text1}");

    // todo: more tests
}
