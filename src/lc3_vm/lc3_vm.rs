#![allow(dead_code)]
use super::constants::{MEMORY_MAX, PC_START};
use super::instruction_table::{FieldInfo, INST_TABLE};
use super::MAX_PROGRAM_SIZE;

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
    TRAP, //TODO: Remove. For now, this will help me to stop the VM execution.
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
    pub fn new(program: [u16; MAX_PROGRAM_SIZE]) -> Self {
        let mut memory: [u16; MEMORY_MAX] = [0u16; MEMORY_MAX];
        memory[(PC_START as usize)..].copy_from_slice(&program);

        let mut vm = LC3VM {
            memory,
            reg: [0; REG_COUNT],
        };

        vm.reg[REG::COND as usize] = FLAGS::ZRO as u16;
        vm.reg[REG::PC as usize] = PC_START;
        vm
    }

    pub fn run(&mut self, debug: bool) {
        loop {
            if debug {
                self.print_state();
            }
            let instruction = self.read_instruction();
            let opcode = self.parse_opcode(instruction);
            println!("Opcode is: {}", opcode);
            match opcode {
                op if op == OP::ADD as u16 => self.process_add(instruction),
                op if op == OP::TRAP as u16 => break,
                _ => panic!("Not implemented"),
            };
        }

        println!("Execution finished");
        self.print_state();
    }

    pub fn print_state(&self) {
        println!("REGISTERS: ");
        for (idx, line) in self.reg.iter().enumerate() {
            println!("[{:0>4}] {}", idx, line);
        }
        println!("");
        println!("MEMORY: ");
        let from = 0; //PC_START as usize;
        let to = from + 64;
        for (idx, line) in self.memory[from..to].iter().enumerate() {
            println!("[{:0>4}] {}", idx + from, line);
        }

        println!("");
        println!("-----------------------------------");
        println!("");
    }

    fn read_instruction(&mut self) -> u16 {
        let pc = &mut self.reg[REG::PC as usize];
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

    fn is_negative(x: u16, bit_count: u16) -> bool {
        (x >> (bit_count - 1) & 1) != 0
    }

    fn sign_extend(x: u16, bit_count: u16) -> u16 {
        if Self::is_negative(x, bit_count) {
            x | (0xFFFF << bit_count)
        } else {
            x
        }
    }

    fn update_flags(&mut self, r_number: usize) {
        let value = &self.reg[r_number];
        println!("Last result is: {value}");
        if *value == 0 {
            self.reg[REG::COND as usize] = FLAGS::ZRO as u16;
        } else if Self::is_negative(*value, 16u16) {
            self.reg[REG::COND as usize] = FLAGS::NEG as u16;
        } else {
            self.reg[REG::COND as usize] = FLAGS::POS as u16;
        }
    }

    // OPERATION HANDLERS

    fn process_add(&mut self, instruction: u16) {
        let dr = Self::get_field_value(instruction, INST_TABLE.ADD.DR);
        let src1_reg = Self::get_field_value(instruction, INST_TABLE.ADD.SR1);
        let is_immediate = Self::get_field_value(instruction, INST_TABLE.ADD.MODE) != 0;

        let src2 = match is_immediate {
            true => {
                let imm = Self::get_field_value(instruction, INST_TABLE.ADD.IMM);
                Self::sign_extend(imm, INST_TABLE.ADD.IMM.size)
            }
            false => {
                let src2_reg = Self::get_field_value(instruction, INST_TABLE.ADD.SR2);
                self.reg[src2_reg as usize]
            }
        };
        let src1 = self.reg[src1_reg as usize];
        self.reg[dr as usize] = ((src1 as u32 + src2 as u32) & 0xFFFF) as u16;
        self.update_flags(dr as usize);
    }
}
