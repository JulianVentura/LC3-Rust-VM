use super::instruction_table::{FieldInfo, INST_TABLE};

const MEMORY_MAX: usize = 1 << 16;
const PC_START: u16 = 0x3000;
//Warning, potential bug.
//It would be nicer to have this directly linked to the REG enum
const REG_COUNT: usize = 11;
enum REG {
    R0 = 0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    PC,
    COND,
    COUNT,
}
enum OP {
    BR = 0,
    ADD,
    LD,
    ST,
    JSR,
    AND,
    LDR,
    STR,
    RTI,
    NOT,
    LDI,
    STI,
    JMP,
    RES,
    LEA,
    TRAP,
}

enum FLAGS {
    POS = 1 << 0,
    ZRO = 1 << 1,
    NEG = 1 << 2,
}
pub struct LC3VM {
    memory: [u16; MEMORY_MAX],
    reg: [u16; REG_COUNT],
}

impl LC3VM {
    pub fn new() -> Self {
        let mut vm = LC3VM {
            memory: [0; MEMORY_MAX],
            reg: [0; REG_COUNT],
        };

        vm.reg[REG::COND as usize] = FLAGS::ZRO as u16;
        vm.memory[REG::PC as usize] = PC_START;
        vm
    }

    pub fn run(&mut self, program: Vec<u16>) {
        loop {
            let instruction = self.read_instruction();
            let opcode = self.parse_opcode(instruction);
            match opcode {
                op if op == OP::ADD as u16 => self.process_add(instruction),
                _ => panic!("Not implemented"),
            };
        }
    }

    fn read_instruction(&mut self) -> u16 {
        let pc = &mut self.reg[REG::COUNT as usize];
        let instruction = self.memory[*pc as usize];
        *pc += 1;

        instruction
    }

    fn parse_opcode(&self, instruction: u16) -> u16 {
        (instruction >> 12) as u16
    }

    fn get_field_value(instruction: u16, field_info: FieldInfo) -> u16 {
        (instruction >> field_info.shift) & field_info.mask
    }

    fn process_add(&mut self, instruction: u16) {
        let dr = Self::get_field_value(instruction, INST_TABLE.ADD.DR);
        let src1_addr = Self::get_field_value(instruction, INST_TABLE.ADD.SR1);
        let is_immediate = Self::get_field_value(instruction, INST_TABLE.ADD.MODE) != 0;

        let src2 = match is_immediate {
            true => Self::get_field_value(instruction, INST_TABLE.ADD.IMM),
            false => {
                let addr = Self::get_field_value(instruction, INST_TABLE.ADD.SR2);
                self.memory[addr as usize]
            }
        };

        self.memory[dr as usize] = self.memory[src1_addr as usize] + src2;
    }
}
