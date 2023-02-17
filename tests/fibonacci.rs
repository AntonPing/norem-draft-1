use std::path::PathBuf;
use std::process;

extern crate norem;
use norem::utils::driver;

#[test]
fn test_fibonacci() {
    let input = PathBuf::from("examples/fibonacci.nrm");
    let library = PathBuf::from("examples/test_lib.c");
    let temp = PathBuf::from("target/examples/fibonacci.temp.c");
    let output = PathBuf::from("target/examples/fibonacci.out");
    driver::run_compile(&input, &temp, false).unwrap();
    driver::run_link(&temp, &library, &output).unwrap();
    let res = process::Command::new("target/examples/fibonacci.out")
        .arg("10")
        .output()
        .unwrap();
    assert!(res.status.success());
    // println!("{}", String::from_utf8_lossy(&res.stdout));
    assert_eq!(res.stdout, Vec::from("55\n"));
}
