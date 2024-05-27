use lc3_rust_vm::lc3_vm;
use lc3_rust_vm::parser;

fn main() {
    let program = parser::parse_image("files/2048.obj");

    let mut vm = lc3_vm::LC3VM::new(program);
    vm.run(false);
}
