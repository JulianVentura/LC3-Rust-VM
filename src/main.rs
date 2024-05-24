use lc3_rust_vm::lc3_vm;
use lc3_rust_vm::programs;

fn main() {
    let program = programs::br_loop_test();

    let mut vm = lc3_vm::LC3VM::new(program);
    vm.run(true);
}
