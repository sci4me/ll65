use crate::binary_writer::BinaryWriter;
use crate::opcodes::OpCode;
use paste;

macro_rules! generate_instructions {
    ( $(($name:ident,$op:ident)),* ) => {
        $(
            pub fn $name(&mut self) {
                self.put_opcode(OpCode::$op);
            }
        )*
    }
}

macro_rules! generate_accumulator_instructions {
    ( $(($name:ident,$op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _accumulator>](&mut self) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _ACC>] });
                }
            }
        )*
    }
}

macro_rules! generate_relative_instructions {
    ( $(($name:ident,$op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _relative>](&mut self, offset: i8) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _REL>] });
                    self.writer.put_i8(offset);
                }
            }
        )*
    }
}

macro_rules! generate_absolute_instructions {
    ( $(($name:ident,$op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _absolute>](&mut self, address: u16) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _AB>] });
                    self.writer.put_u16(address);
                }
            }
        )*
    }
}

macro_rules! generate_indirect_instructions {
    ( $(($name:ident,$op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _indirect>](&mut self, address: u16) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _IN>] });
                    self.writer.put_u16(address);
                }
            }
        )*
    }
}

macro_rules! generate_absolute_x_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _absolute_x>](&mut self, address: u16) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _ABX>] });
                    self.writer.put_u16(address);
                }
            }
        )*
    }
}

macro_rules! generate_absolute_y_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _absolute_y>](&mut self, address: u16) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _ABY>] });
                    self.writer.put_u16(address);
                }
            }
        )*
    }
}

macro_rules! generate_immediate_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _immediate>](&mut self, value: u8) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _IMM>] });
                    self.writer.put_u8(value);
                }
            }
        )*
    }
}

macro_rules! generate_indirect_x_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _indirect_x>](&mut self, value: u8) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _INX>] });
                    self.writer.put_u8(value);
                }
            }
        )*
    }
}

macro_rules! generate_indirect_y_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _indirect_y>](&mut self, value: u8) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _INY>] });
                    self.writer.put_u8(value);
                }
            }
        )*
    }
}

macro_rules! generate_zp_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _zp>](&mut self, address: u8) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _ZP>] });
                    self.writer.put_u8(address);
                }
            }
        )*
    }
}

macro_rules! generate_zpx_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _zpx>](&mut self, address: u8) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _ZPX>] });
                    self.writer.put_u8(address);
                }
            }
        )*
    }
}

macro_rules! generate_zpy_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _zpy>](&mut self, address: u8) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _ZPY>] });
                    self.writer.put_u8(address);
                }
            }
        )*
    }
}

macro_rules! generate_indirect_zp_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _indirect_zp>](&mut self, address: u8) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _INZP>] });
                    self.writer.put_u8(address);
                }
            }
        )*
    }
}

pub struct Assembler {
    writer: BinaryWriter
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            writer: BinaryWriter::new()
        }
    }

    pub fn assemble(&self) -> &[u8] {
        self.writer.as_bytes()
    }

    fn put_opcode(&mut self, opcode: OpCode) {
        self.writer.put_u8(opcode as u8);
    }

    generate_instructions!(
        (brk, BRK),
        (clc, CLC),
        (cld, CLD),
        (cli, CLI),
        (clv, CLV),
        (dex, DEX),
        (dey, DEY),
        (inx, INX),
        (iny, INY),
        (nop, NOP),
        (pha, PHA),
        (php, PHP),
        (pla, PLA),
        (plp, PLP)
    );

    generate_accumulator_instructions!(
        (asl, ASL),
        (dec, DEC),
        (inc, INC),
        (lsr, LSR)
    );

    generate_relative_instructions!(
        (bcc, BCC),
        (bcs, BCS),
        (beq, BEQ),
        (bmi, BMI),
        (bne, BNE),
        (bpl, BPL),
        (bvc, BVC),
        (bvs, BVS)
    );

    generate_absolute_instructions!(
        (adc, ADC),
        (and, AND),
        (asl, ASL),
        (bit, BIT),
        (cmp, CMP),
        (cpx, CPX),
        (cpy, CPY),
        (dec, DEC),
        (eor, EOR),
        (inc, INC),
        (jmp, JMP),
        (jsr, JSR),
        (lda, LDA),
        (ldx, LDX),
        (ldy, LDY),
        (lsr, LSR),
        (ora, ORA)
    );

    generate_indirect_instructions!(
        (jmp, JMP)
    );

    generate_absolute_x_instructions!(
        (adc, ADC),
        (and, AND),
        (asl, ASL),
        (cmp, CMP),
        (dec, DEC),
        (eor, EOR),
        (inc, INC),
        (jmp, JMP),
        (lda, LDA),
        (ldy, LDY),
        (lsr, LSR),
        (ora, ORA)
    );

    generate_absolute_y_instructions!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ldx, LDX),
        (ora, ORA)
    );

    generate_immediate_instructions!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (cpx, CPX),
        (cpy, CPY),
        (eor, EOR),
        (lda, LDA),
        (ldx, LDX),
        (ldy, LDY),
        (ora, ORA)
    );

    generate_indirect_x_instructions!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ora, ORA)
    );

    generate_indirect_y_instructions!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ora, ORA)
    );

    generate_zp_instructions!(
        (adc, ADC),
        (and, AND),
        (asl, ASL),
        (bit, BIT),
        (cmp, CMP),
        (cpx, CPX),
        (cpy, CPY),
        (dec, DEC),
        (eor, EOR),
        (inc, INC),
        (lda, LDA),
        (ldx, LDX),
        (ldy, LDY),
        (lsr, LSR),
        (ora, ORA)
    );

    generate_zpx_instructions!(
        (adc, ADC),
        (and, AND),
        (asl, ASL),
        (cmp, CMP),
        (dec, DEC),
        (eor, EOR),
        (inc, INC),
        (lda, LDA),
        (ldy, LDY),
        (lsr, LSR),
        (ora, ORA)
    );

    generate_zpy_instructions!(
        (ldx, LDX)
    );

    generate_indirect_zp_instructions!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ora, ORA)
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! generate_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _words>]() {
                        let mut subject = Assembler::new();

                        subject.$name();

                        assert_eq!(subject.assemble(), &[OpCode::$op as u8]);
                    }
                }
            )*
        }
    }

    macro_rules! generate_accumulator_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _accumulator_works>]() {
                        let mut subject = Assembler::new();

                        paste::expr! { subject.[<$name _accumulator>](); }

                        assert_eq!(subject.assemble(), paste::expr! { &[OpCode::[<$op _ACC>] as u8] });
                    }
                }
            )*
        }
    }

    macro_rules! generate_relative_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _relative_works>]() {
                        let mut subject = Assembler::new();

                        paste::expr! { subject.[<$name _relative>](-2); }

                        assert_eq!(subject.assemble(), paste::expr! { &[OpCode::[<$op _REL>] as u8, -2i8 as u8] });
                    }
                }
            )*
        }
    }

    macro_rules! generate_absolute_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _absolute_works>]() {
                        let mut subject = Assembler::new();

                        paste::expr! { subject.[<$name _absolute>](256); }

                        assert_eq!(subject.assemble(), paste::expr! { &[OpCode::[<$op _AB>] as u8, 0, 1] });
                    }
                }
            )*
        }
    }

    macro_rules! generate_indirect_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _indirect_works>]() {
                        let mut subject = Assembler::new();

                        paste::expr! { subject.[<$name _indirect>](256); }

                        assert_eq!(subject.assemble(), paste::expr! { &[OpCode::[<$op _IN>] as u8, 0, 1] });
                    }
                }
            )*
        }
    }

    macro_rules! generate_absolute_x_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _absolute_x_works>]() {
                        let mut subject = Assembler::new();

                        paste::expr! { subject.[<$name _absolute_x>](256); }

                        assert_eq!(subject.assemble(), paste::expr! { &[OpCode::[<$op _ABX>] as u8, 0, 1] });
                    }
                }
            )*
        }
    }

    macro_rules! generate_absolute_y_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _absolute_y_works>]() {
                        let mut subject = Assembler::new();

                        paste::expr! { subject.[<$name _absolute_y>](256); }

                        assert_eq!(subject.assemble(), paste::expr! { &[OpCode::[<$op _ABY>] as u8, 0, 1] });
                    }
                }
            )*
        }
    }

    macro_rules! generate_immediate_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _immediate_works>]() {
                        let mut subject = Assembler::new();

                        paste::expr! { subject.[<$name _immediate>](42); }

                        assert_eq!(subject.assemble(), paste::expr! { &[OpCode::[<$op _IMM>] as u8, 42] });
                    }
                }
            )*
        }
    }

    macro_rules! generate_indirect_x_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _indirect_x_works>]() {
                        let mut subject = Assembler::new();

                        paste::expr! { subject.[<$name _indirect_x>](42); }

                        assert_eq!(subject.assemble(), paste::expr! { &[OpCode::[<$op _INX>] as u8, 42] });
                    }
                }
            )*
        }
    }

    macro_rules! generate_indirect_y_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _indirect_y_works>]() {
                        let mut subject = Assembler::new();

                        paste::expr! { subject.[<$name _indirect_y>](42); }

                        assert_eq!(subject.assemble(), paste::expr! { &[OpCode::[<$op _INY>] as u8, 42] });
                    }
                }
            )*
        }
    }

    macro_rules! generate_zp_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _zp_works>]() {
                        let mut subject = Assembler::new();

                        paste::expr! { subject.[<$name _zp>](42); }

                        assert_eq!(subject.assemble(), paste::expr! { &[OpCode::[<$op _ZP>] as u8, 42] });
                    }
                }
            )*
        }
    }

    macro_rules! generate_zpx_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _zpx_works>]() {
                        let mut subject = Assembler::new();

                        paste::expr! { subject.[<$name _zpx>](42); }

                        assert_eq!(subject.assemble(), paste::expr! { &[OpCode::[<$op _ZPX>] as u8, 42] });
                    }
                }
            )*
        }
    }

    macro_rules! generate_zpy_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _zpy_works>]() {
                        let mut subject = Assembler::new();

                        paste::expr! { subject.[<$name _zpy>](42); }

                        assert_eq!(subject.assemble(), paste::expr! { &[OpCode::[<$op _ZPY>] as u8, 42] });
                    }
                }
            )*
        }
    }

    macro_rules! generate_indirect_zp_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _indirect_zp_works>]() {
                        let mut subject = Assembler::new();

                        paste::expr! { subject.[<$name _indirect_zp>](42); }

                        assert_eq!(subject.assemble(), paste::expr! { &[OpCode::[<$op _INZP>] as u8, 42] });
                    }
                }
            )*
        }
    }

    generate_instruction_tests!(
        (brk, BRK),
        (clc, CLC),
        (cld, CLD),
        (cli, CLI),
        (clv, CLV),
        (dex, DEX),
        (dey, DEY),
        (inx, INX),
        (iny, INY),
        (nop, NOP),
        (pha, PHA),
        (php, PHP),
        (pla, PLA),
        (plp, PLP)
    );

    generate_accumulator_instruction_tests!(
        (asl, ASL),
        (dec, DEC),
        (inc, INC),
        (lsr, LSR)
    );

    generate_relative_instruction_tests!(
        (bcc, BCC),
        (bcs, BCS),
        (beq, BEQ),
        (bmi, BMI),
        (bne, BNE),
        (bpl, BPL),
        (bvc, BVC),
        (bvs, BVS)
    );

    generate_absolute_instruction_tests!(
        (adc, ADC),
        (and, AND),
        (asl, ASL),
        (bit, BIT),
        (cmp, CMP),
        (cpx, CPX),
        (cpy, CPY),
        (dec, DEC),
        (eor, EOR),
        (inc, INC),
        (jmp, JMP),
        (jsr, JSR),
        (lda, LDA),
        (ldx, LDX),
        (ldy, LDY),
        (lsr, LSR),
        (ora, ORA)
    );

    generate_indirect_instruction_tests!(
        (jmp, JMP)
    );

    generate_absolute_x_instruction_tests!(
        (adc, ADC),
        (and, AND),
        (asl, ASL),
        (cmp, CMP),
        (dec, DEC),
        (eor, EOR),
        (inc, INC),
        (jmp, JMP),
        (lda, LDA),
        (ldy, LDY),
        (lsr, LSR),
        (ora, ORA)
    );

    generate_absolute_y_instruction_tests!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ldx, LDX),
        (ora, ORA)
    );

    generate_immediate_instruction_tests!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (cpx, CPX),
        (cpy, CPY),
        (eor, EOR),
        (lda, LDA),
        (ldx, LDX),
        (ldy, LDY),
        (ora, ORA)
    );

    generate_indirect_x_instruction_tests!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ora, ORA)
    );

    generate_indirect_y_instruction_tests!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ora, ORA)
    );

    generate_zp_instruction_tests!(
        (adc, ADC),
        (and, AND),
        (asl, ASL),
        (bit, BIT),
        (cmp, CMP),
        (cpx, CPX),
        (cpy, CPY),
        (dec, DEC),
        (eor, EOR),
        (inc, INC),
        (lda, LDA),
        (ldx, LDX),
        (ldy, LDY),
        (lsr, LSR),
        (ora, ORA)
    );

    generate_zpx_instruction_tests!(
        (adc, ADC),
        (and, AND),
        (asl, ASL),
        (cmp, CMP),
        (dec, DEC),
        (eor, EOR),
        (inc, INC),
        (lda, LDA),
        (ldy, LDY),
        (lsr, LSR),
        (ora, ORA)
    );

    generate_zpy_instruction_tests!(
        (ldx, LDX)
    );

    generate_indirect_zp_instruction_tests!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ora, ORA)
    );
}