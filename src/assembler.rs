use crate::binary_writer::BinaryWriter;

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
}