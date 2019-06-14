use crate::asm::opcodes::OpCode;

pub enum NodeKind {
    ResetVector(ResetVectorNode),
    IrqVector(IrqVectorNode),
    NmiVector(NmiVectorNode),
    HighByte(HighByteNode),
    LowByte(LowByteNode),
    Variable(VariableNode),
    Macro(MacroNode),
    If(IfNode),
    For(ForNode),
    Byte(ByteNode),
    Word(WordNode),
    Binary(BinaryNode),
    Unary(UnaryNode),
    Parenthesized(ParenthesizedNode),
    Interpolation(InterpolationNode),
    Label(LabelNode),
    Ident(IdentNode),
    Str(StrNode),
    AbsoluteInstruction(AbsoluteInstructionNode),
    AccumulatorInstruction(AccumulatorInstructionNode),
    IndirectInstruction(IndirectInstructionNode),
    RelativeInstruction(RelativeInstructionNode),
    ZeroPageInstruction(ZeroPageInstructionNode),
}

pub struct ResetVectorNode {
    pub value: Box<NodeKind>,
}

pub struct IrqVectorNode {
    pub value: Box<NodeKind>,
}

pub struct NmiVectorNode {
    pub value: Box<NodeKind>,
}

pub struct HighByteNode {
    pub value: Box<NodeKind>,
}

pub struct LowByteNode {
    pub value: Box<LowByteNode>,
}

pub struct MacroNode {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Vec<Box<NodeKind>>,
}

pub struct IfNode {
    pub condition: Box<NodeKind>,
    pub _true: Vec<Box<NodeKind>>,
    pub _false: Option<Vec<Box<NodeKind>>>
}

pub struct ForNode {
    pub iterator: String,
    pub start: Box<NodeKind>,
    pub end: Box<NodeKind>
}

pub struct VariableNode {
    pub value: String,
}

pub struct LabelNode {
    pub value: String,
}

pub struct IdentNode {
    pub value: String,
}

pub struct ByteNode {
    pub value: u8,
}

pub struct WordNode {
    pub value: u16,
}

#[allow(non_camel_case_types)]
pub enum BinaryOp {
    EQ,
    NE,
    LT,
    GT,
    LTE,
    GTE,
    AND,
    OR,
    BIT_AND,
    BIT_OR,
    BIT_XOR,
}

pub struct BinaryNode {
    pub op: BinaryOp,
    pub left: Box<NodeKind>,
    pub right: Box<NodeKind>
}

#[allow(non_camel_case_types)]
pub enum UnaryOp {
    NEG,
    NOT,
    BIT_NOT,
}

pub struct UnaryNode {
    pub op: UnaryOp,
    pub value: Box<NodeKind>
}

pub struct ParenthesizedNode {
    pub value: Box<NodeKind>
}

pub struct InterpolationNode {
    pub left: Box<NodeKind>,
    pub right: Box<NodeKind>
}

pub struct StrNode {
    pub value: String,
}

pub struct AbsoluteInstructionNode {
    pub op: OpCode,
    pub value: Box<NodeKind>,
}

pub struct AccumulatorInstructionNode {
    pub op: OpCode,
}

pub struct IndirectInstructionNode {
    pub op: OpCode,
    pub value: Box<NodeKind>,
}

pub struct RelativeInstructionNode {
    pub op: OpCode,
    pub value: Box<NodeKind>,
}

pub struct ZeroPageInstructionNode {
    pub op: OpCode,
    pub value: Box<NodeKind>,
}
