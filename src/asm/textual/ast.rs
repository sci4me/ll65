use crate::asm::opcodes::OpCode;

#[derive(Debug)]
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
    EmitByte(EmitByteNode),
    EmitWord(EmitWordNode),
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
#[derive(Debug)]
pub struct ResetVectorNode {
    pub value: Box<NodeKind>,
}

#[derive(Debug)]
pub struct IrqVectorNode {
    pub value: Box<NodeKind>,
}

#[derive(Debug)]
pub struct NmiVectorNode {
    pub value: Box<NodeKind>,
}

#[derive(Debug)]
pub struct HighByteNode {
    pub value: Box<NodeKind>,
}

#[derive(Debug)]
pub struct LowByteNode {
    pub value: Box<LowByteNode>,
}

#[derive(Debug)]
pub struct MacroNode {
    pub name: String,
    pub parameters: Vec<String>,
    pub body: Vec<Box<NodeKind>>,
}

#[derive(Debug)]
pub struct IfNode {
    pub condition: Box<NodeKind>,
    pub _true: Vec<Box<NodeKind>>,
    pub _false: Option<Vec<Box<NodeKind>>>,
}

#[derive(Debug)]
pub struct ForNode {
    pub iterator: String,
    pub start: Box<NodeKind>,
    pub end: Box<NodeKind>,
}

#[derive(Debug)]
pub struct VariableNode {
    pub value: String,
}

#[derive(Debug)]
pub struct LabelNode {
    pub value: String,
}

#[derive(Debug)]
pub struct IdentNode {
    pub value: String,
}

#[derive(Debug)]
pub struct EmitByteNode {
    pub value: u8
}

#[derive(Debug)]
pub struct EmitWordNode {
    pub value: u16
}

#[derive(Debug)]
pub struct ByteNode {
    pub value: u8,
}

#[derive(Debug)]
pub struct WordNode {
    pub value: u16,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct BinaryNode {
    pub op: BinaryOp,
    pub left: Box<NodeKind>,
    pub right: Box<NodeKind>,
}

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum UnaryOp {
    NEG,
    NOT,
    BIT_NOT,
}

#[derive(Debug)]
pub struct UnaryNode {
    pub op: UnaryOp,
    pub value: Box<NodeKind>,
}

#[derive(Debug)]
pub struct ParenthesizedNode {
    pub value: Box<NodeKind>,
}

#[derive(Debug)]
pub struct InterpolationNode {
    pub left: Box<NodeKind>,
    pub right: Box<NodeKind>,
}

#[derive(Debug)]
pub struct StrNode {
    pub value: String,
}

#[derive(Debug)]
pub struct AbsoluteInstructionNode {
    pub op: OpCode,
    pub value: Box<NodeKind>,
}

#[derive(Debug)]
pub struct AccumulatorInstructionNode {
    pub op: OpCode,
}

#[derive(Debug)]
pub struct IndirectInstructionNode {
    pub op: OpCode,
    pub value: Box<NodeKind>,
}

#[derive(Debug)]
pub struct RelativeInstructionNode {
    pub op: OpCode,
    pub value: Box<NodeKind>,
}

#[derive(Debug)]
pub struct ZeroPageInstructionNode {
    pub op: OpCode,
    pub value: Box<NodeKind>,
}
