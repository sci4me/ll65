// Opcode constants that have multiple addressing modes are named in the form
// X_Y, where X is the instruction and Y is the addressing mode. The possible
// addressing modes are:
//       AB:  absolute mode, next two bytes are the low/high byte of an absolute
//            memory address
//      ABX:  absolute,X, next two bytes are added to the value in register X to
//            get the memory address
//      ABY:  same as ABX, except the value of register Y is used instead of X
//      ACC:  accumulator, act on the value in the accumulator
//      IMM:  immediate, next byte is a constant to be used instead of a lookup
//       IN:  indirect, next two bytes are an absolute memory address of the
//            lower nibble of a memory address. That byte and the byte after will
//            be loaded and the address made of those two bytes will be used
//      INX:  (indirect,X) mode, add X to the following byte, modulo 0xFF, and
//            lookup two bytes starting at that location. Those two bytes form
//            the memory address that will be used
//      INY:  (indirect),Y mode, look up two bytes starting at address in the
//            following byte, add Y modulo 0xFFFF, and use the result as an
//            address
//	   INZP:  indirect zero-page, next byte is a memory address within the zero-page
//			  the byte at that address and the byte after it form a memory address
//			  that address is where the actual value is loaded from
//      REL:  relative, next byte contains a signed offset from the current PC.
//       ZP:  zero-page, next byte is the low bits of the memory address (can
//            only address first 256 bytes of memory using ZP)
//      ZPX:  zero-page,X, add next byte to X modulo 0xFF and use that as a
//            memory address

#[allow(non_camel_case_types)]
pub enum OpCode {
    ADC_AB = 0x6D,
    ADC_ABX = 0x7D,
    ADC_ABY = 0x79,
    ADC_IMM = 0x69,
    ADC_INX = 0x61,
    ADC_INY = 0x71,
    ADC_ZP = 0x65,
    ADC_ZPX = 0x75,
    ADC_INZP = 0x72,

    AND_AB = 0x2D,
    AND_ABX = 0x3D,
    AND_ABY = 0x39,
    AND_IMM = 0x29,
    AND_INX = 0x21,
    AND_INY = 0x31,
    AND_ZP = 0x25,
    AND_ZPX = 0x35,
    AND_INZP = 0x32,

    ASL_AB = 0x0E,
    ASL_ABX = 0x1E,
    ASL_ACC = 0x0A,
    ASL_ZP = 0x06,
    ASL_ZPX = 0x16,

    BCC_REL = 0x90,
    BCS_REL = 0xB0,
    BEQ_REL = 0xF0,

    BIT_AB = 0x2C,
    BIT_ABX = 0x3C,
    BIT_ZP = 0x24,
    BIT_ZPX = 0x34,
    BIT_IMM = 0x89,

    BMI_REL = 0x30,
    BNE_REL = 0xD0,
    BPL_REL = 0x10,

    BRK = 0x00,

    BVC_REL = 0x50,
    BVS_REL = 0x70,

    CLC = 0x18,
    CLD = 0xD8,
    CLI = 0x58,
    CLV = 0xB8,

    CMP_AB = 0xCD,
    CMP_ABX = 0xDD,
    CMP_ABY = 0xD9,
    CMP_IMM = 0xC9,
    CMP_INX = 0xC1,
    CMP_INY = 0xD1,
    CMP_ZP = 0xC5,
    CMP_ZPX = 0xD5,
    CMP_INZP = 0xD2,

    CPX_AB = 0xEC,
    CPX_IMM = 0xE0,
    CPX_ZP = 0xE4,

    CPY_AB = 0xCC,
    CPY_IMM = 0xC0,
    CPY_ZP = 0xC4,

    DEC_ACC = 0x3A,
    DEC_AB = 0xCE,
    DEC_ABX = 0xDE,
    DEC_ZP = 0xC6,
    DEC_ZPX = 0xD6,

    DEX = 0xCA,
    DEY = 0x88,

    EOR_AB = 0x4D,
    EOR_ABX = 0x5D,
    EOR_ABY = 0x59,
    EOR_IMM = 0x49,
    EOR_INX = 0x41,
    EOR_INY = 0x51,
    EOR_ZP = 0x45,
    EOR_ZPX = 0x55,
    EOR_INZP = 0x52,

    INC_ACC = 0x1A,
    INC_AB = 0xEE,
    INC_ABX = 0xFE,
    INC_ZP = 0xE6,
    INC_ZPX = 0xF6,

    INX = 0xE8,
    INY = 0xC8,

    JMP_AB = 0x4C,
    JMP_ABX = 0x7C,
    JMP_IN = 0x6C,

    JSR_AB = 0x20,

    LDA_AB = 0xAD,
    LDA_ABX = 0xBD,
    LDA_ABY = 0xB9,
    LDA_IMM = 0xA9,
    LDA_INX = 0xA1,
    LDA_INY = 0xB1,
    LDA_ZP = 0xA5,
    LDA_ZPX = 0xB5,
    LDA_INZP = 0xB2,

    LDX_AB = 0xAE,
    LDX_ABY = 0xBE,
    LDX_IMM = 0xA2,
    LDX_ZP = 0xA6,
    LDX_ZPY = 0xB6,

    LDY_AB = 0xAC,
    LDY_ABX = 0xBC,
    LDY_IMM = 0xA0,
    LDY_ZP = 0xA4,
    LDY_ZPX = 0xB4,

    LSR_AB = 0x4E,
    LSR_ABX = 0x5E,
    LSR_ACC = 0x4A,
    LSR_ZP = 0x46,
    LSR_ZPX = 0x56,

    ORA_IMM = 0x09,
    ORA_ZP = 0x05,
    ORA_ZPX = 0x15,
    ORA_AB = 0x0D,
    ORA_ABX = 0x1D,
    ORA_ABY = 0x19,
    ORA_INX = 0x01,
    ORA_INY = 0x11,
    ORA_INZP = 0x12,

    NOP = 0xEA,

    PHA = 0x48,
    PHP = 0x08,
    PLA = 0x68,
    PLP = 0x28,

    ROL_AB = 0x2E,
    ROL_ABX = 0x3E,
    ROL_ACC = 0x2A,
    ROL_ZP = 0x26,
    ROL_ZPX = 0x36,

    ROR_AB = 0x6E,
    ROR_ABX = 0x7E,
    ROR_ACC = 0x6A,
    ROR_ZP = 0x66,
    ROR_ZPX = 0x76,

    RTI = 0x40,
    RTS = 0x60,

    SBC_IMM = 0xE9,
    SBC_ZP = 0xE5,
    SBC_ZPX = 0xF5,
    SBC_AB = 0xED,
    SBC_ABX = 0xFD,
    SBC_ABY = 0xF9,
    SBC_INX = 0xE1,
    SBC_INY = 0xF1,
    SBC_INZP = 0xF2,

    SEC = 0x38,
    SED = 0xF8,
    SEI = 0x78,

    STA_AB = 0x8D,
    STA_ABX = 0x9D,
    STA_ABY = 0x99,
    STA_INX = 0x81,
    STA_INY = 0x91,
    STA_ZP = 0x85,
    STA_ZPX = 0x95,
    STA_INZP = 0x92,

    STX_ZP = 0x86,
    STX_ZPY = 0x96,
    STX_AB = 0x8E,

    STY_ZP = 0x84,
    STY_ZPX = 0x94,
    STY_AB = 0x8C,

    TAX = 0xAA,
    TAY = 0xA8,
    TSX = 0xBA,
    TXA = 0x8A,
    TXS = 0x9A,
    TYA = 0x98,

    BRA_REL = 0x80,

    BBR0 = 0x0F,
    BBR1 = 0x1F,
    BBR2 = 0x2F,
    BBR3 = 0x3F,
    BBR4 = 0x4F,
    BBR5 = 0x5F,
    BBR6 = 0x6F,
    BBR7 = 0x7F,

    BBS0 = 0x8F,
    BBS1 = 0x9F,
    BBS2 = 0xAF,
    BBS3 = 0xBF,
    BBS4 = 0xCF,
    BBS5 = 0xDF,
    BBS6 = 0xEF,
    BBS7 = 0xFF,

    RMB0 = 0x07,
    RMB1 = 0x17,
    RMB2 = 0x27,
    RMB3 = 0x37,
    RMB4 = 0x47,
    RMB5 = 0x57,
    RMB6 = 0x67,
    RMB7 = 0x77,

    SMB0 = 0x87,
    SMB1 = 0x97,
    SMB2 = 0xA7,
    SMB3 = 0xB7,
    SMB4 = 0xC7,
    SMB5 = 0xD7,
    SMB6 = 0xE7,
    SMB7 = 0xF7,

    TRB_AB = 0x1C,
    TRB_ZP = 0x14,

    TSB_AB = 0x0C,
    TSB_ZP = 0x04,

    STZ_AB = 0x9C,
    STZ_ABX = 0x9E,
    STZ_ZP = 0x64,
    STZ_ZPX = 0x74,

    PHX = 0xDA,
    PHY = 0x5A,
    PLX = 0xFA,
    PLY = 0x7A,

    WAI = 0xCB,
    STP = 0xDB,
}

pub const NMI_VECTOR: u16 = 0xFFFA;
pub const RESET_VECTOR: u16 = 0xFFFC;
pub const IRQ_VECTOR: u16 = 0xFFFE;
