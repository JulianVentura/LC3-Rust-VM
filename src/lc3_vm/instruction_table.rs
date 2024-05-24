#![allow(non_snake_case)]
//This struct stores the information needed to access a field inside an instruction
pub struct FieldInfo {
    pub mask: u16,
    pub shift: u16,
    pub size: u16,
}

//BEGIN INSTRUCTIONS DECLARATION
//Each struct defines the fields inside the corresponding instruction

pub struct ADD {
    pub DR: FieldInfo,
    pub SR1: FieldInfo,
    pub MODE: FieldInfo,
    pub SR2: FieldInfo,
    pub IMM: FieldInfo,
}
pub struct AND {
    pub DR: FieldInfo,
    pub SR1: FieldInfo,
    pub MODE: FieldInfo,
    pub SR2: FieldInfo,
    pub IMM: FieldInfo,
}
pub struct LDI {
    pub DR: FieldInfo,
    pub PCOFFSET: FieldInfo,
}

//BEGIN INSTRUCTIONS DEFINITION
const ADD: ADD = ADD {
    DR: FieldInfo {
        mask: 0x7,
        shift: 9,
        size: 3,
    },
    SR1: FieldInfo {
        mask: 0x7,
        shift: 6,
        size: 3,
    },
    MODE: FieldInfo {
        mask: 0x1,
        shift: 5,
        size: 1,
    },
    SR2: FieldInfo {
        mask: 0x7,
        shift: 0,
        size: 3,
    },
    IMM: FieldInfo {
        mask: 0x1F,
        shift: 0,
        size: 5,
    },
};

const AND: AND = AND {
    DR: FieldInfo {
        mask: 0x7,
        shift: 9,
        size: 3,
    },
    SR1: FieldInfo {
        mask: 0x7,
        shift: 6,
        size: 3,
    },
    MODE: FieldInfo {
        mask: 0x1,
        shift: 5,
        size: 1,
    },
    SR2: FieldInfo {
        mask: 0x7,
        shift: 0,
        size: 3,
    },
    IMM: FieldInfo {
        mask: 0x1F,
        shift: 0,
        size: 5,
    },
};

const LDI: LDI = LDI {
    DR: FieldInfo {
        mask: 0x7,
        shift: 9,
        size: 3,
    },
    PCOFFSET: FieldInfo {
        mask: 0x1FF,
        shift: 0,
        size: 9,
    },
};

//Instruction table aggregates all the instructions inside one struct for easy access
pub struct InstructionTable {
    pub ADD: ADD,
    pub AND: AND,
    pub LDI: LDI,
}
pub const INST_TABLE: InstructionTable = InstructionTable { ADD, AND, LDI };
