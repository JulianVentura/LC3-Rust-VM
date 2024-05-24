use lc3_rust_vm::lc3_vm;

fn binary_to_u16(binary: [u8; 16]) -> u16 {
    let mut value: u16 = 0;
    for (idx, b) in binary.iter().rev().enumerate() {
        value |= (*b as u16) << idx;
    }
    value
}

fn test_add_instruction() {
    let add_instruction: [u8; 16] = [
        0, 0, 0, 1, //opcode
        0, 1, 0, //dr = 2
        0, 0, 1, //src1 = 1
        1, //mode = immediate
        0, 0, 0, 1, 0, //immediate value
    ];

    let converted = binary_to_u16(add_instruction);

    let mut program: [u16; lc3_vm::MAX_PROGRAM_SIZE] = [0u16; lc3_vm::MAX_PROGRAM_SIZE];

    program[0] = converted;

    let vm = lc3_vm::LC3VM::new(program);

    vm.print_state();
}

fn main() {
    test_add_instruction()
}
