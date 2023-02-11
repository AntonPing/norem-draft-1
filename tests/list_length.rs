use std::path::PathBuf;
use std::process;

extern crate norem;
use norem::utils::driver;

#[test]
fn tets_list_length() {
    let input = PathBuf::from("examples/list_length.nrm");
    let library = PathBuf::from("examples/list_length.c");
    let temp = PathBuf::from("target/examples/list_length.temp.c");
    let output = PathBuf::from("target/examples/list_length.out");
    driver::run_compile(&input, &temp, false).unwrap();
    driver::run_link(&temp, &library, &output).unwrap();
    let res = process::Command::new("target/examples/list_length.out")
        .output()
        .unwrap();
    assert_eq!(res.stdout, Vec::from("5\n"));
}
