use crate::asm::binary_writer::BinaryWriter;
use crate::asm::opcodes::OpCode;
use paste;
use rangemap::RangeMap;
use std::collections::HashMap;
use std::fmt;
use std::ops::Range;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum MemoryType {
    RAM,
    ROM,
    IO,
    NONE,
}

impl fmt::Display for MemoryType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MemoryType::RAM => write!(f, "RAM"),
            MemoryType::ROM => write!(f, "ROM"),
            MemoryType::IO => write!(f, "IO"),
            MemoryType::NONE => write!(f, "NONE"),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum Ref {
    Label(u16),
    Address(u16),
}

impl Into<Ref> for u16 {
    fn into(self) -> Ref {
        Ref::Address(self)
    }
}

impl fmt::Display for Ref {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ref::Label(n) => write!(f, "L{}", n),
            Ref::Address(n) => write!(f, "0x{:04X}", n),
        }
    }
}

macro_rules! generate_instructions {
    ( $(($name:ident,$op:ident)),* ) => {
        $(
            pub fn $name(&mut self) -> Result<(), String> {
                self.put_opcode(OpCode::$op)
            }
        )*
    }
}

macro_rules! generate_accumulator_instructions {
    ( $(($name:ident,$op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _accumulator>](&mut self) -> Result<(), String> {
                    self.put_opcode(paste::expr! { OpCode::[<$op _ACC>] })
                }
            }
        )*
    }
}

macro_rules! generate_relative_instructions {
    ( $(($name:ident,$op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _relative>]<T: Into<Ref>>(&mut self, address: T) -> Result<(), String> {
                    self.put_opcode(paste::expr! { OpCode::[<$op _REL>] })?;
                    self.put_relative_address(address.into())
                }

                pub fn [<$name _relative_immediate>](&mut self, offset: i8) -> Result<(), String> {
                    self.put_opcode(paste::expr! { OpCode::[<$op _REL>] })?;
                    self.safe_put_u8(offset as u8).map(|_| ())
                }
            }
        )*
    }
}

macro_rules! generate_absolute_instructions {
    ( $(($name:ident,$op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _absolute>]<T: Into<Ref>>(&mut self, address: T) -> Result<(), String> {
                    self.put_opcode(paste::expr! { OpCode::[<$op _AB>] })?;
                    self.put_address(address.into()).map(|_| ())
                }
            }
        )*
    }
}

macro_rules! generate_indirect_instructions {
    ( $(($name:ident,$op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _indirect>]<T: Into<Ref>>(&mut self, address: T) -> Result<(), String> {
                    self.put_opcode(paste::expr! { OpCode::[<$op _IN>] })?;
                    self.put_address(address.into()).map(|_| ())
                }
            }
        )*
    }
}

macro_rules! generate_absolute_x_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _absolute_x>]<T: Into<Ref>>(&mut self, address: T) -> Result<(), String> {
                    self.put_opcode(paste::expr! { OpCode::[<$op _ABX>] })?;
                    self.put_address(address.into()).map(|_| ())
                }
            }
        )*
    }
}

macro_rules! generate_absolute_y_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _absolute_y>]<T: Into<Ref>>(&mut self, address: T) -> Result<(), String> {
                    self.put_opcode(paste::expr! { OpCode::[<$op _ABY>] })?;
                    self.put_address(address.into()).map(|_| ())
                }
            }
        )*
    }
}

macro_rules! generate_immediate_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _immediate>](&mut self, value: u8) -> Result<(), String> {
                    self.put_opcode(paste::expr! { OpCode::[<$op _IMM>] })?;
                    self.safe_put_u8(value).map(|_| ())
                }
            }
        )*
    }
}

macro_rules! generate_indirect_x_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _indirect_x>](&mut self, value: u8) -> Result<(), String> {
                    self.put_opcode(paste::expr! { OpCode::[<$op _INX>] })?;
                    self.safe_put_u8(value).map(|_| ())
                }
            }
        )*
    }
}

macro_rules! generate_indirect_y_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _indirect_y>](&mut self, value: u8) -> Result<(), String> {
                    self.put_opcode(paste::expr! { OpCode::[<$op _INY>] })?;
                    self.safe_put_u8(value).map(|_| ())
                }
            }
        )*
    }
}

macro_rules! generate_zp_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _zp>](&mut self, address: u8) -> Result<(), String> {
                    self.put_opcode(paste::expr! { OpCode::[<$op _ZP>] })?;
                    self.safe_put_u8(address).map(|_| ())
                }
            }
        )*
    }
}

macro_rules! generate_zpx_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _zpx>](&mut self, address: u8) -> Result<(), String> {
                    self.put_opcode(paste::expr! { OpCode::[<$op _ZPX>] })?;
                    self.safe_put_u8(address).map(|_| ())
                }
            }
        )*
    }
}

macro_rules! generate_zpy_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _zpy>](&mut self, address: u8) -> Result<(), String> {
                    self.put_opcode(paste::expr! { OpCode::[<$op _ZPY>] })?;
                    self.safe_put_u8(address).map(|_| ())
                }
            }
        )*
    }
}

macro_rules! generate_indirect_zp_instructions {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            paste::item! {
                pub fn [<$name _indirect_zp>](&mut self, address: u8) -> Result<(), String> {
                    self.put_opcode(paste::expr! { OpCode::[<$op _INZP>] })?;
                    self.safe_put_u8(address).map(|_| ())
                }
            }
        )*
    }
}

macro_rules! generate_bbr_style_instruction {
    ( $(($name:ident, $op:ident)),* ) => {
        $(
            pub fn $name(&mut self, bit: u8) -> Result<(), String> {
                match bit {
                    0 => self.put_opcode(paste::expr! { OpCode::[<$op 0>] }),
                    1 => self.put_opcode(paste::expr! { OpCode::[<$op 1>] }),
                    2 => self.put_opcode(paste::expr! { OpCode::[<$op 2>] }),
                    3 => self.put_opcode(paste::expr! { OpCode::[<$op 3>] }),
                    4 => self.put_opcode(paste::expr! { OpCode::[<$op 4>] }),
                    5 => self.put_opcode(paste::expr! { OpCode::[<$op 5>] }),
                    6 => self.put_opcode(paste::expr! { OpCode::[<$op 6>] }),
                    7 => self.put_opcode(paste::expr! { OpCode::[<$op 7>] }),
                    _ => Err(format!("Expected bit value 0-7, got {}", bit))
                }
            }
        )*
    };
}

pub struct Assembler {
    writer: BinaryWriter,
    next_label: u16,
    patch_locations: HashMap<Ref, Vec<u16>>,
    relative_patch_locations: HashMap<Ref, Vec<u16>>,
    label_locations: HashMap<Ref, u16>,
    memory_layout: RangeMap<u16, MemoryType>,
}

impl Assembler {
    pub fn new(capacity: usize) -> Self {
        let mut result = Self {
            writer: BinaryWriter::new(capacity),
            next_label: 0,
            patch_locations: HashMap::new(),
            relative_patch_locations: HashMap::new(),
            label_locations: HashMap::new(),
            memory_layout: RangeMap::new(),
        };
        result.build_default_memory_layout();
        result
    }

    fn build_default_memory_layout(&mut self) {
        let end = (self.capacity() - 1) as u16;
        self.memory_layout.insert(0..end, MemoryType::ROM);
    }

    fn check_memory_type(&self, address: u16, expected: Vec<MemoryType>) -> Result<(), String> {
        let t = match self.memory_layout.get(&address) {
            Some(v) => *v,
            None => return Err(format!("Address out of bounds: {:04X}", address)),
        };

        if !expected.contains(&t) {
            return Err(format!(
                "Expected {} at {}, got {}",
                expected
                    .iter()
                    .map(|v| v.to_string())
                    .fold(String::new(), |a, b| format!("{}, {}", a, b)),
                address,
                t
            ));
        }

        Ok(())
    }

    fn safe_put_u8(&mut self, v: u8) -> Result<usize, String> {
        self.check_memory_type(self.cursor(), vec![MemoryType::ROM])?;
        self.writer.put_u8(v)
    }

    fn safe_put_u16(&mut self, v: u16) -> Result<usize, String> {
        self.check_memory_type(self.cursor(), vec![MemoryType::ROM])?;
        self.writer.put_u16(v)
    }

    fn put_address(&mut self, address: Ref) -> Result<(), String> {
        match address {
            Ref::Label(_) => {
                let placeholder = self.safe_put_u16(0)?;
                self.mark_patch_location(address, placeholder as u16);
            }
            Ref::Address(address) => {
                self.safe_put_u16(address).map(|_| ())?;
            }
        }
        Ok(())
    }

    fn put_relative_address(&mut self, address: Ref) -> Result<(), String> {
        let placeholder = self.safe_put_u8(0)?;
        match address {
            Ref::Label(_) => {
                self.mark_relative_patch_location(address, placeholder as u16);
            }
            Ref::Address(address) => {
                self.patch_relative(address, placeholder as u16);
            }
        }
        Ok(())
    }

    fn mark_patch_location(&mut self, label: Ref, address: u16) {
        match self.patch_locations.get_mut(&label) {
            Some(locations) => {
                locations.push(address);
            }
            None => {
                self.patch_locations.insert(label, vec![address]);
            }
        }
    }

    fn mark_relative_patch_location(&mut self, label: Ref, address: u16) {
        match self.relative_patch_locations.get_mut(&label) {
            Some(locations) => {
                locations.push(address);
            }
            None => {
                self.relative_patch_locations.insert(label, vec![address]);
            }
        }
    }

    fn patch_relative(&mut self, label_location: u16, address: u16) {
        let offset = i64::from(label_location) - i64::from(address) - 1;
        self.writer
            .set_u8(address as usize, offset as u8)
            .expect("Internal Error: Unable to set i8");
    }

    fn put_opcode(&mut self, opcode: OpCode) -> Result<(), String> {
        self.safe_put_u8(opcode as u8).map(|_| ())
    }

    fn fixup_patches(&mut self) {
        for (label, locations) in &self.patch_locations {
            let address = self
                .label_locations
                .get(label)
                .expect("Internal Error: Unable to retrieve label location");
            for location in locations {
                self.writer
                    .set_u16(*location as usize, *address)
                    .expect("Internal Error: Unable to set u16");
            }
        }
    }

    fn fixup_relative_patches(&mut self) {
        let mut relative_patches = Vec::new();

        for (label, locations) in &self.relative_patch_locations {
            let address = self
                .label_locations
                .get(label)
                .expect("Internal Error: Unable to retrieve label location");
            for location in locations {
                relative_patches.push((*address, *location));
            }
        }

        for (address, location) in relative_patches {
            self.patch_relative(address, location);
        }
    }

    fn check_for_unmarked_labels(&self) -> Result<(), String> {
        if self.next_label as usize != self.label_locations.len() {
            let labels: Vec<u16> = self
                .patch_locations
                .keys()
                .chain(self.relative_patch_locations.keys())
                .filter_map(|r| match r {
                    Ref::Label(x) => Some(*x),
                    _ => None,
                })
                .collect();

            for index in labels {
                let label = Ref::Label(index);

                if !self.label_locations.contains_key(&label) {
                    return Err(format!("Unmarked label: {}", label));
                }
            }
        }
        Ok(())
    }

    pub fn assemble(&mut self) -> Result<&[u8], String> {
        self.check_for_unmarked_labels()?;
        self.fixup_patches();
        self.fixup_relative_patches();
        Ok(self.writer.as_bytes())
    }

    pub fn capacity(&self) -> usize {
        self.writer.capacity()
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
        self.label_locations
            .insert(result, address)
            .expect("Internal Error: Unable to insert into label_locations");
        result
    }

    pub fn resolve(&self, label: Ref) -> Option<u16> {
        match label {
            Ref::Label(_) => self.label_locations.get(&label).cloned(),
            Ref::Address(x) => Some(x),
        }
    }

    pub fn mark(&mut self, label: Ref) -> Result<(), String> {
        if self.label_locations.contains_key(&label) {
            Err(format!("Label {} alread marked!", label))
        } else {
            self.label_locations.insert(label, self.cursor());
            Ok(())
        }
    }

    pub fn set_u8(&mut self, address: u16, value: u8) -> Result<(), String> {
        self.check_memory_type(address, vec![MemoryType::ROM])?;
        self.writer.set_u8(address as usize, value)
    }

    pub fn set_u16(&mut self, address: u16, value: u16) -> Result<(), String> {
        self.check_memory_type(address, vec![MemoryType::ROM])?;
        self.writer.set_u16(address as usize, value)
    }

    pub fn set_memory_type(&mut self, range: Range<u16>, t: MemoryType) {
        self.memory_layout.insert(range, t);
    }

    pub fn get_memory_type(&self, address: u16) -> Result<MemoryType, String> {
        match self.memory_layout.get(&address) {
            Some(v) => Ok(*v),
            None => Err(format!("Address out of bounds: {:04X}", address))
        }
    }

    pub fn org(&mut self, address: u16) -> Result<(), String> {
        self.writer.set_cursor(address as usize)
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

    generate_indirect_instructions!((jmp, JMP));

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

    generate_zpy_instructions!((ldx, LDX), (stx, STX));

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

    generate_bbr_style_instruction!((bbr, BBR), (bbs, BBS), (rmb, RMB), (smb, SMB));
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::zero_vec_of_len;

    #[test]
    fn u16_into_ref_works() {
        let result: Ref = 1234.into();

        assert_eq!(result, Ref::Address(1234));
    }

    #[test]
    fn ref_display_works() {
        assert_eq!(&format!("{}", Ref::Label(42)), "L42");
        assert_eq!(&format!("{}", Ref::Address(0x1234)), "0x1234");
    }

    macro_rules! generate_instruction_tests {
        ( $(($name:ident, $op:ident)),* ) => {
            $(
                paste::item! {
                    #[test]
                    fn [<$name _works>]() {
                        let mut subject = Assembler::new(0x100);

                        subject.$name().unwrap();

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = OpCode::$op as u8;

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
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
                        let mut subject = Assembler::new(0x100);

                        paste::expr! { subject.[<$name _accumulator>]().unwrap(); }

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = paste::expr! { OpCode::[<$op _ACC>] as u8 };

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
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
                        let mut subject = Assembler::new(0x100);

                        let l = subject.label();
                        subject.mark(l).unwrap();

                        paste::expr! { subject.[<$name _relative>](l).unwrap(); }

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = paste::expr! { OpCode::[<$op _REL>] as u8 };
                        expected[1] = -2i8 as u8;

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
                    }
                }

                paste::item! {
                    #[test]
                    fn [<$name _relative_immediate_works>]() {
                        let mut subject = Assembler::new(0x100);

                        paste::expr! { subject.[<$name _relative_immediate>](-2).unwrap(); }

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = paste::expr! { OpCode::[<$op _REL>] as u8 };
                        expected[1] = -2i8 as u8;

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
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
                        let mut subject = Assembler::new(0x100);

                        paste::expr! { subject.[<$name _absolute>](256).unwrap(); }

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = paste::expr! { OpCode::[<$op _AB>] as u8 };
                        expected[1] = 0;
                        expected[2] = 1;

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
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
                        let mut subject = Assembler::new(0x100);

                        paste::expr! { subject.[<$name _indirect>](256).unwrap(); }

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = paste::expr! { OpCode::[<$op _IN>] as u8 };
                        expected[1] = 0;
                        expected[2] = 1;

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
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
                        let mut subject = Assembler::new(0x100);

                        paste::expr! { subject.[<$name _absolute_x>](256).unwrap(); }

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = paste::expr! { OpCode::[<$op _ABX>] as u8 };
                        expected[1] = 0;
                        expected[2] = 1;

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
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
                        let mut subject = Assembler::new(0x100);

                        paste::expr! { subject.[<$name _absolute_y>](256).unwrap(); }

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = paste::expr! { OpCode::[<$op _ABY>] as u8 };
                        expected[1] = 0;
                        expected[2] = 1;

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
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
                        let mut subject = Assembler::new(0x100);

                        paste::expr! { subject.[<$name _immediate>](42).unwrap(); }

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = paste::expr! { OpCode::[<$op _IMM>] as u8 };
                        expected[1] = 42;

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
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
                        let mut subject = Assembler::new(0x100);

                        paste::expr! { subject.[<$name _indirect_x>](42).unwrap(); }

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = paste::expr! { OpCode::[<$op _INX>] as u8 };
                        expected[1] = 42;

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
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
                        let mut subject = Assembler::new(0x100);

                        paste::expr! { subject.[<$name _indirect_y>](42).unwrap(); }

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = paste::expr! { OpCode::[<$op _INY>] as u8 };
                        expected[1] = 42;

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
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
                        let mut subject = Assembler::new(0x100);

                        paste::expr! { subject.[<$name _zp>](42).unwrap(); }

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = paste::expr! { OpCode::[<$op _ZP>] as u8 };
                        expected[1] = 42;

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
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
                        let mut subject = Assembler::new(0x100);

                        paste::expr! { subject.[<$name _zpx>](42).unwrap(); }

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = paste::expr! { OpCode::[<$op _ZPX>] as u8 };
                        expected[1] = 42;

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
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
                        let mut subject = Assembler::new(0x100);

                        paste::expr! { subject.[<$name _zpy>](42).unwrap(); }

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = paste::expr! { OpCode::[<$op _ZPY>] as u8 };
                        expected[1] = 42;

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
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
                        let mut subject = Assembler::new(0x100);

                        paste::expr! { subject.[<$name _indirect_zp>](42).unwrap(); }

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = paste::expr! { OpCode::[<$op _INZP>] as u8 };
                        expected[1] = 42;

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
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
                        let mut subject = Assembler::new(0x100);

                        subject.$name(0).unwrap();
                        subject.$name(1).unwrap();
                        subject.$name(2).unwrap();
                        subject.$name(3).unwrap();
                        subject.$name(4).unwrap();
                        subject.$name(5).unwrap();
                        subject.$name(6).unwrap();
                        subject.$name(7).unwrap();

                        let mut expected = zero_vec_of_len(0x100);
                        expected[0] = paste::expr! { OpCode::[<$op 0>] as u8 };
                        expected[1] = paste::expr! { OpCode::[<$op 1>] as u8 };
                        expected[2] = paste::expr! { OpCode::[<$op 2>] as u8 };
                        expected[3] = paste::expr! { OpCode::[<$op 3>] as u8 };
                        expected[4] = paste::expr! { OpCode::[<$op 4>] as u8 };
                        expected[5] = paste::expr! { OpCode::[<$op 5>] as u8 };
                        expected[6] = paste::expr! { OpCode::[<$op 6>] as u8 };
                        expected[7] = paste::expr! { OpCode::[<$op 7>] as u8 };

                        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
                    }

                    #[test]
                    fn [<$name _panics_if_parameter_is_out_of_bounds>]() {
                        let mut subject = Assembler::new(0x100);

                        assert_eq!(subject.$name(8), Err(String::from("Expected bit value 0-7, got 8")))
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

    generate_indirect_instruction_tests!((jmp, JMP));

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

    generate_zpy_instruction_tests!((ldx, LDX), (stx, STX));

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

    generate_bbr_style_instruction_tests!((bbr, BBR), (bbs, BBS), (rmb, RMB), (smb, SMB));

    #[test]
    fn labels_work() {
        let mut subject = Assembler::new(0x100);

        subject.sei().unwrap();

        let label = subject.label();
        subject.jmp_absolute(label).unwrap();

        subject.cli().unwrap();

        subject.mark(label).unwrap();

        let mut expected = zero_vec_of_len(0x100);
        expected[0] = OpCode::SEI as u8;
        expected[1] = OpCode::JMP_AB as u8;
        expected[2] = 5;
        expected[3] = 0;
        expected[4] = OpCode::CLI as u8;

        assert_eq!(subject.assemble(), Ok(expected.as_slice()));
    }

    #[test]
    fn unused_labels_are_allowed() {
        let mut subject = Assembler::new(0x100);

        subject.label();

        assert_eq!(subject.assemble(), Ok(zero_vec_of_len(0x100).as_slice()));
    }

    #[test]
    fn labels_complain_when_unmarked() {
        let mut subject = Assembler::new(0x100);

        let l = subject.label();
        subject.jmp_absolute(l).unwrap();

        assert_eq!(subject.assemble(), Err(String::from("Unmarked label: L0")));
    }
}
