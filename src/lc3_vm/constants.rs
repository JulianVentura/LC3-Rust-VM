pub const MEMORY_MAX: usize = 1 << 16;
pub const PC_START: u16 = 0x32;
pub const MAX_PROGRAM_SIZE: usize = MEMORY_MAX - PC_START as usize;
