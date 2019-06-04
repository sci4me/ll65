use crate::binary_writer::BinaryWriter;
use crate::opcodes::OpCode;

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

    pub fn adc_absolute(&mut self, address: u16) {
        self.writer.put_u8(OpCode::ADC_AB as u8);
        self.writer.put_u16(address);
    }

    pub fn adc_absolute_x(&mut self, address: u16) {
        self.writer.put_u8(OpCode::ADC_ABX as u8);
        self.writer.put_u16(address);
    }

    pub fn adc_absolute_y(&mut self, address: u16) {
        self.writer.put_u8(OpCode::ADC_ABY as u8);
        self.writer.put_u16(address);
    }

    pub fn adc_immediate(&mut self, value: u8) {
        self.writer.put_u8(OpCode::ADC_IMM as u8);
        self.writer.put_u8(value);
    }

    pub fn adc_indirect_x(&mut self, value: u8) {
        self.writer.put_u8(OpCode::ADC_INX as u8);
        self.writer.put_u8(value);
    }

    pub fn adc_indirect_y(&mut self, value: u8) {
        self.writer.put_u8(OpCode::ADC_INY as u8);
        self.writer.put_u8(value);
    }

    pub fn adc_zp(&mut self, address: u8) {
        self.writer.put_u8(OpCode::ADC_ZP as u8);
        self.writer.put_u8(address);
    }

    pub fn adc_zpx(&mut self, address: u8) {
        self.writer.put_u8(OpCode::ADC_ZPX as u8);
        self.writer.put_u8(address);
    }

    pub fn adc_indirect_zp(&mut self, address: u8) {
        self.writer.put_u8(OpCode::ADC_INZP as u8);
        self.writer.put_u8(address);
    }
    
    pub fn and_absolute(&mut self, address: u16) {
        self.writer.put_u8(OpCode::AND_AB as u8);
        self.writer.put_u16(address);
    }

    pub fn and_absolute_x(&mut self, address: u16) {
        self.writer.put_u8(OpCode::AND_ABX as u8);
        self.writer.put_u16(address);
    }

    pub fn and_absolute_y(&mut self, address: u16) {
        self.writer.put_u8(OpCode::AND_ABY as u8);
        self.writer.put_u16(address);
    }

    pub fn and_immediate(&mut self, value: u8) {
        self.writer.put_u8(OpCode::AND_IMM as u8);
        self.writer.put_u8(value);
    }

    pub fn and_indirect_x(&mut self, value: u8) {
        self.writer.put_u8(OpCode::AND_INX as u8);
        self.writer.put_u8(value);
    }

    pub fn and_indirect_y(&mut self, value: u8) {
        self.writer.put_u8(OpCode::AND_INY as u8);
        self.writer.put_u8(value);
    }

    pub fn and_zp(&mut self, address: u8) {
        self.writer.put_u8(OpCode::AND_ZP as u8);
        self.writer.put_u8(address);
    }

    pub fn and_zpx(&mut self, address: u8) {
        self.writer.put_u8(OpCode::AND_ZPX as u8);
        self.writer.put_u8(address);
    }

    pub fn and_indirect_zp(&mut self, address: u8) {
        self.writer.put_u8(OpCode::AND_INZP as u8);
        self.writer.put_u8(address);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adc_absolute_works() {
        let mut subject = Assembler::new();

        subject.adc_absolute(256);

        assert_eq!(subject.assemble(), &[OpCode::ADC_AB as u8, 0, 1]);
    }

    #[test]
    fn adc_absolute_x_works() {
        let mut subject = Assembler::new();

        subject.adc_absolute_x(256);

        assert_eq!(subject.assemble(), &[OpCode::ADC_ABX as u8, 0, 1]);
    }

    #[test]
    fn adc_absolute_y_works() {
        let mut subject = Assembler::new();

        subject.adc_absolute_y(256);

        assert_eq!(subject.assemble(), &[OpCode::ADC_ABY as u8, 0, 1]);
    }

    #[test]
    fn adc_immediate_works() {
        let mut subject = Assembler::new();

        subject.adc_immediate(42);

        assert_eq!(subject.assemble(), &[OpCode::ADC_IMM as u8, 42]);
    }

    #[test]
    fn adc_indirect_x_works() {
        let mut subject = Assembler::new();

        subject.adc_indirect_x(42);

        assert_eq!(subject.assemble(), &[OpCode::ADC_INX as u8, 42]);
    }

    #[test]
    fn adc_indirect_y_works() {
        let mut subject = Assembler::new();

        subject.adc_absolute_y(256);

        assert_eq!(subject.assemble(), &[OpCode::ADC_ABY as u8, 0, 1]);
    }

    #[test]
    fn adc_zp_works() {
        let mut subject = Assembler::new();

        subject.adc_zp(42);

        assert_eq!(subject.assemble(), &[OpCode::ADC_ZP as u8, 42]);
    }

    #[test]
    fn adc_zpx_works() {
        let mut subject = Assembler::new();

        subject.adc_zpx(42);

        assert_eq!(subject.assemble(), &[OpCode::ADC_ZPX as u8, 42]);
    }

    #[test]
    fn adc_indirect_zp_works() {
        let mut subject = Assembler::new();

        subject.adc_indirect_zp(42);

        assert_eq!(subject.assemble(), &[OpCode::ADC_INZP as u8, 42]);
    }

    #[test]
    fn and_absolute_works() {
        let mut subject = Assembler::new();

        subject.and_absolute(256);

        assert_eq!(subject.assemble(), &[OpCode::AND_AB as u8, 0, 1]);
    }

    #[test]
    fn and_absolute_x_works() {
        let mut subject = Assembler::new();

        subject.and_absolute_x(256);

        assert_eq!(subject.assemble(), &[OpCode::AND_ABX as u8, 0, 1]);
    }

    #[test]
    fn and_absolute_y_works() {
        let mut subject = Assembler::new();

        subject.and_absolute_y(256);

        assert_eq!(subject.assemble(), &[OpCode::AND_ABY as u8, 0, 1]);
    }

    #[test]
    fn and_immediate_works() {
        let mut subject = Assembler::new();

        subject.and_immediate(42);

        assert_eq!(subject.assemble(), &[OpCode::AND_IMM as u8, 42]);
    }

    #[test]
    fn and_indirect_x_works() {
        let mut subject = Assembler::new();

        subject.and_indirect_x(42);

        assert_eq!(subject.assemble(), &[OpCode::AND_INX as u8, 42]);
    }

    #[test]
    fn and_indirect_y_works() {
        let mut subject = Assembler::new();

        subject.and_absolute_y(256);

        assert_eq!(subject.assemble(), &[OpCode::AND_ABY as u8, 0, 1]);
    }

    #[test]
    fn and_zp_works() {
        let mut subject = Assembler::new();

        subject.and_zp(42);

        assert_eq!(subject.assemble(), &[OpCode::AND_ZP as u8, 42]);
    }

    #[test]
    fn and_zpx_works() {
        let mut subject = Assembler::new();

        subject.and_zpx(42);

        assert_eq!(subject.assemble(), &[OpCode::AND_ZPX as u8, 42]);
    }

    #[test]
    fn and_indirect_zp_works() {
        let mut subject = Assembler::new();

        subject.and_indirect_zp(42);

        assert_eq!(subject.assemble(), &[OpCode::AND_INZP as u8, 42]);
    }
}