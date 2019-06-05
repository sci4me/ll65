pub mod binary_writer;
pub mod opcodes;
pub mod assembler;


#[cfg(test)]
mod tests {
    use super::assembler;
    use std::fs::File;
    use std::io::Write;

    #[test]
    fn test() {
        let mut asm = assembler::Assembler::new();

        const NMI_VECTOR: u16 = 0xFFFA;
        const RESET_VECTOR: u16 = 0xFFFC;
        const IRQ_VECTOR: u16 = 0xFFFE;

        asm.sei();

        let entry = asm.len();

        asm.set_u16(RESET_VECTOR, entry).unwrap();

        let mut file = File::create("out.bin").unwrap();
        file.write(asm.assemble()).unwrap();
        file.flush();
    }
}