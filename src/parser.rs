use std::fs::File;
use std::io::{ErrorKind, Read};
use std::path::Path;

use crate::lc3_vm::MAX_PROGRAM_SIZE;

pub fn parse_image(path: &str) -> [u16; MAX_PROGRAM_SIZE] {
    println!("PARSER BEGIN");
    let mut file = File::open(&Path::new(path)).expect("Error reading file");
    let mut accum: [u8; 2] = [0u8; 2];
    file.read_exact(&mut accum).expect("Couldn't read origin");
    let origin = u16::from_be_bytes(accum);
    println!("{:#0x}", origin);
    let max_read = MAX_PROGRAM_SIZE - origin as usize;
    let mut program: [u16; MAX_PROGRAM_SIZE] = [0u16; MAX_PROGRAM_SIZE];

    let mut lines_read = 0;
    for idx in 0..max_read {
        if let Err(e) = file.read_exact(&mut accum) {
            match e.kind() {
                ErrorKind::UnexpectedEof => break,
                _ => panic!("Error reading input file"),
            }
        }
        program[idx] = u16::from_be_bytes(accum);
        lines_read += 1;
    }

    println!("Lines read: {lines_read}");
    program
}
