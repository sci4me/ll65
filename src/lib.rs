pub mod binary_writer;
pub mod opcodes;
pub mod assembler;
pub(crate) mod utils;

#[cfg(test)]
mod tests {
    use super::assembler::*;
    use super::opcodes::*;
    use std::fs::File;
    use std::io::Write;

    #[test]
    fn test() {
        let mut asm = Assembler::new();

        let nmi = asm.cursor();
        asm.rti();

        let irq = asm.cursor();
        asm.rti();

        let reset = asm.cursor();
        asm.sei();



        asm.set_u16(NMI_VECTOR, nmi).unwrap();
        asm.set_u16(RESET_VECTOR, reset).unwrap();
        asm.set_u16(IRQ_VECTOR, irq).unwrap();

        let mut file = File::create("out.bin").unwrap();
        file.write(asm.assemble()).unwrap();
        file.flush().unwrap();
    }
}