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
pub struct NOT {
    pub DR: FieldInfo,
    pub SR: FieldInfo,
}
pub struct LDI {
    pub DR: FieldInfo,
    pub PCOFFSET: FieldInfo,
}
pub struct LD {
    pub DR: FieldInfo,
    pub PCOFFSET: FieldInfo,
}
pub struct LDR {
    pub DR: FieldInfo,
    pub BASER: FieldInfo,
    pub OFFSET: FieldInfo,
}
pub struct LEA {
    pub DR: FieldInfo,
    pub OFFSET: FieldInfo,
}
pub struct ST {
    pub SR: FieldInfo,
    pub OFFSET: FieldInfo,
}
pub struct STI {
    pub SR: FieldInfo,
    pub OFFSET: FieldInfo,
}
pub struct STR {
    pub SR: FieldInfo,
    pub BASE: FieldInfo,
    pub OFFSET: FieldInfo,
}
pub struct BR {
    pub CONDFL: FieldInfo,
    pub PCOFFSET: FieldInfo,
}
pub struct JMP {
    pub BASE: FieldInfo,
}
pub struct JSR {
    pub LONGFL: FieldInfo,
    pub PCOFFSET: FieldInfo,
    pub BASER: FieldInfo,
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

const NOT: NOT = NOT {
    DR: FieldInfo {
        mask: 0x7,
        shift: 9,
        size: 3,
    },
    SR: FieldInfo {
        mask: 0x7,
        shift: 6,
        size: 3,
    },
};

const BR: BR = BR {
    CONDFL: FieldInfo {
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

const LD: LD = LD {
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

const LDR: LDR = LDR {
    DR: FieldInfo {
        mask: 0x7,
        shift: 9,
        size: 3,
    },
    BASER: FieldInfo {
        mask: 0x7,
        shift: 0,
        size: 3,
    },
    OFFSET: FieldInfo {
        mask: 0x1F,
        shift: 0,
        size: 6,
    },
};

const LEA: LEA = LEA {
    DR: FieldInfo {
        mask: 0x7,
        shift: 9,
        size: 3,
    },
    OFFSET: FieldInfo {
        mask: 0x1FF,
        shift: 0,
        size: 9,
    },
};

const ST: ST = ST {
    SR: FieldInfo {
        mask: 0x7,
        shift: 9,
        size: 3,
    },
    OFFSET: FieldInfo {
        mask: 0x1FF,
        shift: 0,
        size: 9,
    },
};

const STR: STR = STR {
    SR: FieldInfo {
        mask: 0x7,
        shift: 9,
        size: 3,
    },
    BASE: FieldInfo {
        mask: 0x7,
        shift: 6,
        size: 3,
    },
    OFFSET: FieldInfo {
        mask: 0x3F,
        shift: 0,
        size: 6,
    },
};

const STI: STI = STI {
    SR: FieldInfo {
        mask: 0x7,
        shift: 9,
        size: 3,
    },
    OFFSET: FieldInfo {
        mask: 0x1FF,
        shift: 0,
        size: 9,
    },
};

const JMP: JMP = JMP {
    BASE: FieldInfo {
        mask: 0x7,
        shift: 6,
        size: 3,
    },
};

const JSR: JSR = JSR {
    LONGFL: FieldInfo {
        mask: 0x1,
        shift: 11,
        size: 1,
    },
    PCOFFSET: FieldInfo {
        mask: 0x3FF,
        shift: 0,
        size: 11,
    },
    BASER: FieldInfo {
        mask: 0x7,
        shift: 6,
        size: 3,
    },
};

//Instruction table aggregates all the instructions inside one struct for easy access
pub struct InstructionTable {
    pub ADD: ADD,
    pub AND: AND,
    pub NOT: NOT,
    pub BR: BR,
    pub JMP: JMP,
    pub JSR: JSR,
    pub LDI: LDI,
    pub LD: LD,
    pub LDR: LDR,
    pub LEA: LEA,
    pub ST: ST,
    pub STI: STI,
    pub STR: STR,
}
pub const INST_TABLE: InstructionTable = InstructionTable {
    ADD,
    AND,
    NOT,
    BR,
    JMP,
    JSR,
    LDI,
    LD,
    LDR,
    LEA,
    ST,
    STI,
    STR,
};
