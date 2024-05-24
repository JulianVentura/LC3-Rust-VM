use super::lc3_vm;

fn binary_to_u16(binary: &[u8; 16]) -> u16 {
    let mut value: u16 = 0;
    for (idx, b) in binary.iter().rev().enumerate() {
        value |= (*b as u16) << idx;
    }
    value
}

pub fn swap16(value: u16) -> u16 {
    value << 8 | value >> 8
}

fn construct_program(raw_program: &[[u8; 16]]) -> [u16; lc3_vm::MAX_PROGRAM_SIZE] {
    let mut program: [u16; lc3_vm::MAX_PROGRAM_SIZE] = [0u16; lc3_vm::MAX_PROGRAM_SIZE];

    let program_len = raw_program.len();
    for i in 0..program_len {
        program[i] = binary_to_u16(&raw_program[i]);
    }

    program
}

pub fn add_full_test() -> [u16; lc3_vm::MAX_PROGRAM_SIZE] {
    let set_register_a: [u8; 16] = [
        0, 0, 0, 1, //opcode
        0, 0, 1, //dr = 1
        0, 0, 1, //src1 = 1
        1, //mode = immediate
        0, 0, 0, 0, 1, //immediate_v = 1
    ];

    let set_register_b: [u8; 16] = [
        0, 0, 0, 1, //opcode
        0, 1, 0, //dr = 2
        0, 1, 0, //src1 = 2
        1, //mode = immediate
        1, 1, 1, 1, 1, //immediate_v = -1
    ];

    let add_registers: [u8; 16] = [
        0, 0, 0, 1, //opcode
        0, 1, 1, //dr = 2
        0, 0, 1, //src1 = 1
        0, //mode = add2
        0, 0, //empty bits
        0, 1, 0, //src2 = 2
    ];

    let stop_execution: [u8; 16] = [
        1, 1, 1, 1, //opcode
        0, 0, 0, //
        0, 0, 0, //
        0, //
        0, 0, 0, 0, 0, //
    ];

    let raw_program = [
        set_register_a, //
        set_register_b, //
        add_registers,  //
        stop_execution, //
    ];

    construct_program(&raw_program)
}

pub fn ldi_full_test() -> [u16; lc3_vm::MAX_PROGRAM_SIZE] {
    let nop: [u8; 16] = [
        0, 0, 0, 1, //opcode
        0, 0, 1, //dr = 1
        0, 0, 1, //src1 = 1
        1, //mode = immediate
        0, 0, 0, 0, 0, //immediate_v = 1
    ];

    let ldi: [u8; 16] = [
        1, 0, 1, 0, //opcode
        0, 0, 0, //dr = 0
        0, 0, 0, 0, 0, 0, 1, 0, 0, //pcoffset = 4
    ];

    // PC_START + 5 = 64 + 5 = 69
    let value_address: [u8; 16] = [
        0, 0, 0, 0, 0, 0, 0, 0, //
        0, 0, 1, 0, 0, 1, 0, 1, //
    ];

    // 23
    let value: [u8; 16] = [
        0, 0, 0, 0, 0, 0, 0, 0, //
        0, 0, 0, 1, 0, 1, 1, 1, //
    ];

    let stop_execution: [u8; 16] = [
        1, 1, 1, 1, //opcode
        0, 0, 0, //
        0, 0, 0, //
        0, //
        0, 0, 0, 0, 0, //
    ];

    /*
    [
        64: LDI R0 4
        65: NOP
        66: NOP
        67: STOP
        68: 69
        69: 23
    ]
    */

    let raw_program = [
        ldi,            //
        nop,            //
        nop,            //
        stop_execution, //
        value_address,
        value,
    ];

    construct_program(&raw_program)
}
