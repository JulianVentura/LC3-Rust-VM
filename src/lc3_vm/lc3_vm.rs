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
            match opcode {
                op if op == OP::ADD as u16 => self.process_add(instruction),
                op if op == OP::AND as u16 => self.process_and(instruction),
                op if op == OP::NOT as u16 => self.process_not(instruction),
                op if op == OP::BR as u16 => self.process_br(instruction),
                op if op == OP::JMP as u16 => self.process_jmp(instruction),
                op if op == OP::JSR as u16 => self.process_jsr(instruction),
                op if op == OP::LDI as u16 => self.process_ldi(instruction),
                op if op == OP::LD as u16 => self.process_ld(instruction),
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
        let to = from + 100;
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
        self.reg[dr as usize] = Self::sum(src1, src2);
        self.update_flags(dr as usize);
    }

    fn process_ldi(&mut self, instruction: u16) {
        let dr = Self::get_field_value(instruction, INST_TABLE.LDI.DR);
        let pc_off = Self::get_field_value(instruction, INST_TABLE.LDI.PCOFFSET);
        let extended_pc_off = Self::sign_extend(pc_off, INST_TABLE.LDI.PCOFFSET.size);
        let address = Self::sum(self.reg[REG::PC as usize], extended_pc_off);
        self.reg[dr as usize] = self.memory[self.memory[address as usize] as usize];
        self.update_flags(dr as usize);
    }

    fn process_and(&mut self, instruction: u16) {
        let dr = Self::get_field_value(instruction, INST_TABLE.AND.DR);
        let src1_reg = Self::get_field_value(instruction, INST_TABLE.AND.SR1);
        let is_immediate = Self::get_field_value(instruction, INST_TABLE.AND.MODE) != 0;

        let src2 = match is_immediate {
            true => {
                let imm = Self::get_field_value(instruction, INST_TABLE.AND.IMM);
                Self::sign_extend(imm, INST_TABLE.AND.IMM.size)
            }
            false => {
                let src2_reg = Self::get_field_value(instruction, INST_TABLE.AND.SR2);
                self.reg[src2_reg as usize]
            }
        };

        let src1 = self.reg[src1_reg as usize];
        self.reg[dr as usize] = ((src1 as u32 & src2 as u32) & 0xFFFF) as u16;
        self.update_flags(dr as usize);
    }

    fn process_not(&mut self, instruction: u16) {
        let dr = Self::get_field_value(instruction, INST_TABLE.NOT.DR);
        let src_reg = Self::get_field_value(instruction, INST_TABLE.NOT.SR);
        self.reg[dr as usize] = !self.reg[src_reg as usize];
        self.update_flags(dr as usize);
    }

    fn process_br(&mut self, instruction: u16) {
        let cond_flag = Self::get_field_value(instruction, INST_TABLE.BR.CONDFL);
        let pc_off = Self::get_field_value(instruction, INST_TABLE.BR.PCOFFSET);
        let extended_pc_off = Self::sign_extend(pc_off, INST_TABLE.LDI.PCOFFSET.size);
        let jump = (cond_flag & self.reg[REG::COND as usize]) != 0;

        if jump {
            self.reg[REG::PC as usize] = Self::sum(self.reg[REG::PC as usize], extended_pc_off);
        }
    }

    fn process_jmp(&mut self, instruction: u16) {
        let reg = Self::get_field_value(instruction, INST_TABLE.JMP.BASE);
        self.reg[REG::PC as usize] = self.reg[reg as usize]
    }

    fn process_jsr(&mut self, instruction: u16) {
        let is_imm = Self::get_field_value(instruction, INST_TABLE.JSR.LONGFL) != 0;
        self.reg[REG::R7 as usize] = self.reg[REG::PC as usize];
        let src2 = match is_imm {
            true => {
                let pc_off = Self::get_field_value(instruction, INST_TABLE.JSR.PCOFFSET);
                Self::sign_extend(pc_off, INST_TABLE.JSR.PCOFFSET.size)
            }
            false => {
                let src2_reg = Self::get_field_value(instruction, INST_TABLE.JSR.BASER);
                self.reg[src2_reg as usize]
            }
        };

        self.reg[REG::PC as usize] = Self::sum(self.reg[REG::PC as usize], src2);
    }

    fn process_ld(&mut self, instruction: u16) {
        let dr = Self::get_field_value(instruction, INST_TABLE.LD.DR);
        let pc_off = Self::get_field_value(instruction, INST_TABLE.LD.PCOFFSET);
        let extended_pc_off = Self::sign_extend(pc_off, INST_TABLE.LD.PCOFFSET.size);
        let address = Self::sum(self.reg[REG::PC as usize], extended_pc_off);
        self.reg[dr as usize] = self.memory[address as usize];
        self.update_flags(dr as usize);
    }

    fn sum(a: u16, b: u16) -> u16 {
        ((a as u32 + b as u32) & 0xFFFF) as u16
    }
}
