pub mod assembler;
mod binary_writer;
pub mod opcodes;
pub mod textual;

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::prelude::*;

    use super::assembler::{Assembler, MemoryType};
    use super::opcodes::*;

    fn emit_code(asm: &mut Assembler) -> Result<(), String> {
        asm.set_memory_type(0x0000..0x7FFF, MemoryType::RAM);
        asm.set_memory_type(0x8000..0xFFFF, MemoryType::ROM);

        asm.org(0x8000)?;

        let irq = asm.label();
        let nmi = asm.label();
        let reset = asm.label();

        asm.mark(irq)?; {
            asm.rti()?;
        }

        asm.mark(nmi)?; {
            asm.rti()?;
        }

        asm.mark(reset)?; {
            let l = asm.label();
            asm.mark(l)?;
            asm.nop()?;
            asm.jmp_absolute(l)?;
        }

        asm.set_u16(IRQ_VECTOR, asm.resolve(irq).unwrap())?;
        asm.set_u16(NMI_VECTOR, asm.resolve(nmi).unwrap())?;
        asm.set_u16(RESET_VECTOR, asm.resolve(reset).unwrap())?;

        Ok(())
    }

    #[test]
    fn test() {
        let mut asm = Assembler::new(0x10000);

        match emit_code(&mut asm) {
            Ok(_) => {},
            Err(e) => {
                eprintln!("{}", e);
                panic!("Failed!");
            }
        }

        let assembled = match asm.assemble() {
            Ok(r) => r,
            Err(e) => {
                eprintln!("{}", e);
                panic!("Failed!");
            }
        };

        let mut file = File::create("test.bin").unwrap();
        file.write_all(&assembled[0x8000..]).unwrap();
    }
}