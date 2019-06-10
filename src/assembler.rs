use crate::binary_writer::BinaryWriter;
use crate::opcodes::OpCode;
use std::collections::HashMap;
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
                pub fn [<$name _relative>](&mut self, address: &Ref) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _REL>] });
                    self.put_relative_address(address);
                }

                pub fn [<$name _relative_immediate>](&mut self, offset: i8) {
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
                pub fn [<$name _absolute>](&mut self, address: &Ref) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _AB>] });
                    self.put_address(address);
                }
            }
        )*
    }
}

macro_rules! generate_indirect_instructions {
    ( $(($name:ident,$op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _indirect>](&mut self, address: &Ref) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _IN>] });
                    self.put_address(address);
                }
            }
        )*
    }
}

macro_rules! generate_absolute_x_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _absolute_x>](&mut self, address: &Ref) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _ABX>] });
                    self.put_address(address);
                }
            }
        )*
    }
}

macro_rules! generate_absolute_y_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _absolute_y>](&mut self, address: &Ref) {
                    self.put_opcode(paste::expr! { OpCode::[<$op _ABY>] });
                    self.put_address(address);
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

macro_rules! generate_bbr_style_instruction {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            pub fn $name(&mut self, bit: u8) {
                if bit > 7 {
                    panic!("Expected bit value 0-7, got {}", bit);
                }
                match bit {
                    0 => self.put_opcode(paste::expr! { OpCode::[<$op 0>] }),
                    1 => self.put_opcode(paste::expr! { OpCode::[<$op 1>] }),
                    2 => self.put_opcode(paste::expr! { OpCode::[<$op 2>] }),
                    3 => self.put_opcode(paste::expr! { OpCode::[<$op 3>] }),
                    4 => self.put_opcode(paste::expr! { OpCode::[<$op 4>] }),
                    5 => self.put_opcode(paste::expr! { OpCode::[<$op 5>] }),
                    6 => self.put_opcode(paste::expr! { OpCode::[<$op 6>] }),
                    7 => self.put_opcode(paste::expr! { OpCode::[<$op 7>] }),
                    _ => unreachable!()
                }
            }
        )*
    };
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum Ref {
    Label(u16),
    Address(u16)
}

pub struct Assembler {
    writer: BinaryWriter,
    next_label: u16,
    patch_locations: HashMap<Ref, Vec<u16>>,
    relative_patch_locations: HashMap<Ref, Vec<u16>>,
    label_locations: HashMap<Ref, u16>
}

impl Assembler {
    pub fn new() -> Self {
        Self {
            writer: BinaryWriter::new(0x10000),
            next_label: 0,
            patch_locations: HashMap::new(),
            relative_patch_locations: HashMap::new(),
            label_locations: HashMap::new()
        }
    }

    fn put_address(&mut self, address:&Ref) {
        match address {
            Ref::Label(_) => {
                let placeholder = self.writer.put_u16(0);
                self.mark_patch_location(address.clone(), placeholder as u16);
            },
            Ref::Address(address) => {
                self.writer.put_u16(*address);
            }
        }
    }

    fn put_relative_address(&mut self, address: &Ref) {
        let placeholder = self.writer.put_i8(0);
        match address {
            Ref::Label(_) => {
                self.mark_relative_patch_location(address.clone(), placeholder as u16);
            },
            Ref::Address(address) => {
                self.patch_relative(*address, placeholder as u16);
            }
        }
    }

    fn mark_patch_location(&mut self, label: Ref, address: u16) {
        match self.patch_locations.get_mut(&label) {
            Some(locations) => { locations.push(address); },
            None => { self.patch_locations.insert(label, vec![address]); }
        }
    }
    
    fn mark_relative_patch_location(&mut self, label: Ref, address: u16) {
        match self.relative_patch_locations.get_mut(&label) {
            Some(locations) => { locations.push(address); },
            None => { self.relative_patch_locations.insert(label, vec![address]); }
        }
    }

    fn patch_relative(&mut self, label_location: u16, address: u16) {
        let offset = label_location as i64 - address as i64 - 1;

        if offset < -128 || offset > 127 {
            panic!("Relative offset too far: {}", offset);
        }

        self.writer.set_i8(address as usize, offset as i8).unwrap();
    }

    fn put_opcode(&mut self, opcode: OpCode) {
        self.writer.put_u8(opcode as u8);
    }

    fn fixup_patches(&mut self) {
        for (label, locations) in &self.patch_locations {
            if let Some(address) = self.label_locations.get(label) {
                for location in locations {
                    self.writer.set_u16(*location as usize, *address).unwrap();
                }
            } else {
                panic!("Unmarked label: {:?}", label);
            }
        }
    }

    fn fixup_relative_patches(&mut self) {
        let mut relative_patches = Vec::new();

        for (label, locations) in &self.relative_patch_locations {
            if let Some(address) = self.label_locations.get(label) {
                for location in locations {
                    relative_patches.push((*address, *location));
                }
            } else {
                panic!("Unmarked label: {:?}", label);
            }
        }

        for (address, location) in relative_patches {
            self.patch_relative(address, location);
        }
    }

    pub fn assemble(&mut self) -> &[u8] {
        self.fixup_patches();
        self.fixup_relative_patches();
        self.writer.as_bytes()
    }

    pub fn len(&self) -> usize {
        self.writer.len()
    }

    pub fn cursor(&self) -> u16 {
        self.writer.cursor() as u16
    }

    pub fn label(&mut self) -> Ref {
        let result = self.next_label;
        self.next_label += 1;
        Ref::Label(result)
    }    

    pub fn label_at(&mut self, address: u16) -> Ref {
        let result = self.label();
        self.label_locations.insert(result.clone(), address).unwrap();
        result
    }

    pub fn resolve(&self, label: &Ref) -> Option<u16> {
        self.label_locations.get(label).map(|x| *x)
    }

    pub fn mark(&mut self, label: &Ref) {
        self.label_locations.insert(label.clone(), self.cursor());
    }

    pub fn set_u8(&mut self, address: u16, value: u8) -> Result<(), String> {
        self.writer.set_u8(address as usize, value)
    }

    pub fn set_u16(&mut self, address: u16, value: u16) -> Result<(), String> {
        self.writer.set_u16(address as usize, value)
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
        (plp, PLP),
        (rti, RTI),
        (rts, RTS),
        (sec, SEC),
        (sed, SED),
        (sei, SEI),
        (tax, TAX),
        (tay, TAY),
        (tsx, TSX),
        (txa, TXA),
        (txs, TXS),
        (tya, TYA),
        (phx, PHX),
        (phy, PHY),
        (plx, PLX),
        (ply, PLY),
        (wai, WAI),
        (stp, STP)
    );

    generate_accumulator_instructions!(
        (asl, ASL),
        (dec, DEC),
        (inc, INC),
        (lsr, LSR),
        (rol, ROL),
        (ror, ROR)
    );

    generate_relative_instructions!(
        (bcc, BCC),
        (bcs, BCS),
        (beq, BEQ),
        (bmi, BMI),
        (bne, BNE),
        (bpl, BPL),
        (bvc, BVC),
        (bvs, BVS),
        (bra, BRA)
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
        (ora, ORA),
        (rol, ROL),
        (ror, ROR),
        (sbc, SBC),
        (sta, STA),
        (stx, STX),
        (sty, STY),
        (trb, TRB),
        (tsb, TSB),
        (stz, STZ)
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
        (ora, ORA),
        (rol, ROL),
        (ror, ROR),
        (sbc, SBC),
        (sta, STA),
        (stz, STZ),
        (bit, BIT)
    );

    generate_absolute_y_instructions!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ldx, LDX),
        (ora, ORA),
        (sbc, SBC),
        (sta, STA)
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
        (ora, ORA),
        (sbc, SBC),
        (bit, BIT)
    );

    generate_indirect_x_instructions!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ora, ORA),
        (sbc, SBC),
        (sta, STA)
    );

    generate_indirect_y_instructions!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ora, ORA),
        (sbc, SBC),
        (sta, STA)
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
        (ora, ORA),
        (rol, ROL),
        (ror, ROR),
        (sbc, SBC),
        (sta, STA),
        (stx, STX),
        (sty, STY),
        (trb, TRB),
        (tsb, TSB),
        (stz, STZ)
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
        (ora, ORA),
        (rol, ROL),
        (ror, ROR),
        (sbc, SBC),
        (sta, STA),
        (sty, STY),
        (stz, STZ),
        (bit, BIT)
    );

    generate_zpy_instructions!(
        (ldx, LDX),
        (stx, STX)
    );

    generate_indirect_zp_instructions!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ora, ORA),
        (sbc, SBC),
        (sta, STA)
    );

    generate_bbr_style_instruction!(
        (bbr, BBR),
        (bbs, BBS),
        (rmb, RMB),
        (smb, SMB)
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::zero_vec_of_len;

    macro_rules! generate_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _works>]() {
                        let mut subject = Assembler::new();

                        subject.$name();

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = OpCode::$op as u8;

                        assert_eq!(subject.assemble(), expected.as_slice());
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

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = paste::expr! { OpCode::[<$op _ACC>] as u8 };

                        assert_eq!(subject.assemble(), expected.as_slice());
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

                        let l = subject.label();
                        subject.mark(&l);
                        
                        paste::expr! { subject.[<$name _relative>](&l); }

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = paste::expr! { OpCode::[<$op _REL>] as u8 };
                        expected[1] = -2i8 as u8;

                        assert_eq!(subject.assemble(), expected.as_slice()); 
                    }
                }

                paste::item! {
                    #[test]
                    fn [<$name _relative_immediate_works>]() {
                        let mut subject = Assembler::new();

                        paste::expr! { subject.[<$name _relative_immediate>](-2); }

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = paste::expr! { OpCode::[<$op _REL>] as u8 };
                        expected[1] = -2i8 as u8;

                        assert_eq!(subject.assemble(), expected.as_slice());
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

                        paste::expr! { subject.[<$name _absolute>](&Ref::Address(256)); }

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = paste::expr! { OpCode::[<$op _AB>] as u8 };
                        expected[1] = 0;
                        expected[2] = 1;

                        assert_eq!(subject.assemble(), expected.as_slice());
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

                        paste::expr! { subject.[<$name _indirect>](&Ref::Address(256)); }

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = paste::expr! { OpCode::[<$op _IN>] as u8 };
                        expected[1] = 0;
                        expected[2] = 1;

                        assert_eq!(subject.assemble(), expected.as_slice());
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

                        paste::expr! { subject.[<$name _absolute_x>](&Ref::Address(256)); }

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = paste::expr! { OpCode::[<$op _ABX>] as u8 };
                        expected[1] = 0;
                        expected[2] = 1;

                        assert_eq!(subject.assemble(), expected.as_slice());
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

                        paste::expr! { subject.[<$name _absolute_y>](&Ref::Address(256)); }

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = paste::expr! { OpCode::[<$op _ABY>] as u8 };
                        expected[1] = 0;
                        expected[2] = 1;

                        assert_eq!(subject.assemble(), expected.as_slice());
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

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = paste::expr! { OpCode::[<$op _IMM>] as u8 };
                        expected[1] = 42;

                        assert_eq!(subject.assemble(), expected.as_slice());
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

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = paste::expr! { OpCode::[<$op _INX>] as u8 };
                        expected[1] = 42;

                        assert_eq!(subject.assemble(), expected.as_slice());
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

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = paste::expr! { OpCode::[<$op _INY>] as u8 };
                        expected[1] = 42;

                        assert_eq!(subject.assemble(), expected.as_slice());
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

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = paste::expr! { OpCode::[<$op _ZP>] as u8 };
                        expected[1] = 42;

                        assert_eq!(subject.assemble(), expected.as_slice());
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

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = paste::expr! { OpCode::[<$op _ZPX>] as u8 };
                        expected[1] = 42;

                        assert_eq!(subject.assemble(), expected.as_slice());
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

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = paste::expr! { OpCode::[<$op _ZPY>] as u8 };
                        expected[1] = 42;

                        assert_eq!(subject.assemble(), expected.as_slice());
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

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = paste::expr! { OpCode::[<$op _INZP>] as u8 };
                        expected[1] = 42;

                        assert_eq!(subject.assemble(), expected.as_slice());
                    }
                }
            )*
        }
    }

    macro_rules! generate_bbr_style_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _works>]() {
                        let mut subject = Assembler::new();

                        subject.$name(0);
                        subject.$name(1);
                        subject.$name(2);
                        subject.$name(3);
                        subject.$name(4);
                        subject.$name(5);
                        subject.$name(6);
                        subject.$name(7);

                        let mut expected = zero_vec_of_len(subject.len() as usize);
                        expected[0] = paste::expr! { OpCode::[<$op 0>] as u8 };
                        expected[1] = paste::expr! { OpCode::[<$op 1>] as u8 };
                        expected[2] = paste::expr! { OpCode::[<$op 2>] as u8 };
                        expected[3] = paste::expr! { OpCode::[<$op 3>] as u8 };
                        expected[4] = paste::expr! { OpCode::[<$op 4>] as u8 };
                        expected[5] = paste::expr! { OpCode::[<$op 5>] as u8 };
                        expected[6] = paste::expr! { OpCode::[<$op 6>] as u8 };
                        expected[7] = paste::expr! { OpCode::[<$op 7>] as u8 };

                        assert_eq!(subject.assemble(), expected.as_slice());
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
        (plp, PLP),
        (rti, RTI),
        (rts, RTS),
        (sec, SEC),
        (sed, SED),
        (sei, SEI),
        (tax, TAX),
        (tay, TAY),
        (tsx, TSX),
        (txa, TXA),
        (txs, TXS),
        (tya, TYA),
        (phx, PHX),
        (phy, PHY),
        (plx, PLX),
        (ply, PLY),
        (wai, WAI),
        (stp, STP)
    );

    generate_accumulator_instruction_tests!(
        (asl, ASL),
        (dec, DEC),
        (inc, INC),
        (lsr, LSR),
        (rol, ROL),
        (ror, ROR)
    );

    generate_relative_instruction_tests!(
        (bcc, BCC),
        (bcs, BCS),
        (beq, BEQ),
        (bmi, BMI),
        (bne, BNE),
        (bpl, BPL),
        (bvc, BVC),
        (bvs, BVS),
        (bra, BRA)
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
        (ora, ORA),
        (rol, ROL),
        (ror, ROR),
        (sbc, SBC),
        (sta, STA),
        (stx, STX),
        (sty, STY),
        (trb, TRB),
        (tsb, TSB)
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
        (ora, ORA),
        (rol, ROL),
        (ror, ROR),
        (sbc, SBC),
        (sta, STA),
        (stz, STZ),
        (bit, BIT)
    );

    generate_absolute_y_instruction_tests!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ldx, LDX),
        (ora, ORA),
        (sbc, SBC),
        (sta, STA)
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
        (ora, ORA),
        (sbc, SBC),
        (bit, BIT)
    );

    generate_indirect_x_instruction_tests!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ora, ORA),
        (sbc, SBC),
        (sta, STA)
    );

    generate_indirect_y_instruction_tests!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ora, ORA),
        (sbc, SBC),
        (sta, STA)
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
        (ora, ORA),
        (rol, ROL),
        (ror, ROR),
        (sbc, SBC),
        (sta, STA),
        (stx, STX),
        (sty, STY),
        (trb, TRB),
        (tsb, TSB)
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
        (ora, ORA),
        (rol, ROL),
        (ror, ROR),
        (sbc, SBC),
        (sta, STA),
        (sty, STY),
        (stz, STZ),
        (bit, BIT)
    );

    generate_zpy_instruction_tests!(
        (ldx, LDX),
        (stx, STX)
    );

    generate_indirect_zp_instruction_tests!(
        (adc, ADC),
        (and, AND),
        (cmp, CMP),
        (eor, EOR),
        (lda, LDA),
        (ora, ORA),
        (sbc, SBC),
        (sta, STA)
    );

    generate_bbr_style_instruction_tests!(
        (bbr, BBR),
        (bbs, BBS),
        (rmb, RMB),
        (smb, SMB)
    );

    #[test]
    fn labels_work() {
        let mut subject = Assembler::new();

        subject.sei();

        let label = subject.label();
        subject.jmp_absolute(&label);

        subject.cli();

        subject.mark(&label);

        let mut expected = zero_vec_of_len(subject.len());
        expected[0] = OpCode::SEI as u8;
        expected[1] = OpCode::JMP_AB as u8;
        expected[2] = 5;
        expected[3] = 0;
        expected[4] = OpCode::CLI as u8;

        assert_eq!(subject.assemble(), expected.as_slice());
    }
}