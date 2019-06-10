pub mod assembler;
pub mod binary_writer;
pub mod opcodes;
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

        let nmi = asm.label();
        asm.jsr_absolute(nmi);
        asm.rti();

        let irq = asm.label();
        asm.jsr_absolute(irq);
        asm.rti();

        let reset = asm.cursor();
        let main = asm.label();
        asm.jmp_absolute(main);

        {
            asm.mark(nmi);
            asm.rts();
        }

        {
            asm.mark(irq);
            asm.rts();
        }

        {
            asm.mark(main);
            asm.sei();

            asm.lda_immediate(3);
            asm.sta_absolute(0xFF02);

            let l = asm.label();
            asm.mark(l);
            asm.lda_absolute(0xFEE8);
            asm.inc_accumulator();
            asm.and_immediate(7);
            asm.sta_absolute(0xFEE8);

            asm.lda_immediate(65);
            asm.sta_absolute(0xFF00);
            asm.lda_immediate(10);
            asm.sta_absolute(0xFF00);
            asm.jmp_absolute(l);

            // asm.lda_immediate(2);
            // asm.sta_absolute(0xFF02);
            // asm.lda_immediate(7);
            // asm.sta_absolute(0xFEE8);

            // asm.ldx_immediate(0);
            // let l = asm.label();
            // asm.mark(&l);
            //     asm.lda_absolute(0xFEE8);
            //     asm.inc_accumulator();
            //     asm.and_immediate(7);
            //     asm.sta_absolute(0xFEE8);

            //     asm.lda_immediate(65);
            //     asm.sta_absolute_x(0xFB00);
            //     asm.inx();
            //     asm.bne_relative(&l);
        }

        asm.set_u16(NMI_VECTOR, asm.resolve(nmi).unwrap()).unwrap();
        asm.set_u16(IRQ_VECTOR, asm.resolve(irq).unwrap()).unwrap();
        asm.set_u16(RESET_VECTOR, reset).unwrap();

        let mut file = File::create("out.bin").unwrap();
        file.write(asm.assemble()).unwrap();
        file.flush().unwrap();
    }
}
