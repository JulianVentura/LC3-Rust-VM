pub const MEMORY_MAX: usize = 0x3000 + 256;
pub const PC_START: u16 = 0x3000;
pub const MAX_PROGRAM_SIZE: usize = MEMORY_MAX - PC_START as usize;
