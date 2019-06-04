pub struct BinaryWriter {
    data: Vec<u8>
}

impl BinaryWriter {
    pub fn new() -> Self {
        Self {
            data: Vec::new()
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.data.as_slice()
    }

    pub fn put_u8(&mut self, value: u8) -> usize {
        self.data.push(value);
        self.data.len() - 1
    }

    pub fn put_u16(&mut self, value: u16) -> usize {
        let index = self.data.len();
        self.data.push((value & 0xFF) as u8);
        self.data.push(((value >> 8) & 0xFF) as u8);
        index
    }

    pub fn set_u8(&mut self, index: usize, value: u8) -> Result<(), String> {
        if index >= self.data.len() {
            Err(format!("Index out of bounds: {}", index))
        } else {
            self.data[index] = value;
            Ok(())
        }
    }

    pub fn set_u16(&mut self, index: usize, value: u16) -> Result<(), String> {
        if index + 1 >= self.data.len() {
            Err(format!("Index out of bounds: {}", index))
        } else {
            self.data[index] = (value & 0xFF) as u8;
            self.data[index + 1] = ((value >> 8) & 0xFF) as u8;
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn len_works() {
        let mut subject = BinaryWriter::new();

        subject.put_u8(42);

        assert_eq!(subject.len(), 1);

        subject.put_u16(256);

        assert_eq!(subject.len(), 3);
    }

    #[test]
    fn as_bytes_works() {
        let mut subject = BinaryWriter::new();

        subject.put_u8(42);
        subject.put_u16(256);

        assert_eq!(subject.as_bytes(), &[42u8, 0u8, 1u8]);
    }

    #[test]
    fn put_u8_works() {
        let mut subject = BinaryWriter::new();

        assert_eq!(subject.put_u8(42), 0);
        assert_eq!(subject.put_u8(24), 1);

        assert_eq!(subject.data[0], 42);
        assert_eq!(subject.data[1], 24);
    }

    #[test]
    fn put_u16_works() {
        let mut subject = BinaryWriter::new();

        assert_eq!(subject.put_u16(256), 0);

        assert_eq!(subject.data.len(), 2);
        assert_eq!(subject.data[0], 0);
        assert_eq!(subject.data[1], 1);
    }

    #[test]
    fn set_u8_works() {
        let mut subject = BinaryWriter::new();

        assert_eq!(subject.put_u8(42), 0);

        assert_eq!(subject.set_u8(0, 24), Ok(()));

        assert_eq!(subject.data[0], 24);
    }

    #[test]
    fn set_u8_returns_an_error_if_index_is_out_of_bounds() {
        let mut subject = BinaryWriter::new();

        assert_eq!(subject.set_u8(0, 24), Err(String::from("Index out of bounds: 0")));
    }

    #[test]
    fn set_u16_works() {
        let mut subject = BinaryWriter::new();

        assert_eq!(subject.put_u16(256), 0);

        assert_eq!(subject.set_u16(0, 24), Ok(()));

        assert_eq!(subject.data[0], 24);
        assert_eq!(subject.data[1], 0);
    }

    #[test]
    fn set_u16_returns_an_error_if_index_is_out_of_bounds() {
        let mut subject = BinaryWriter::new();

        subject.put_u8(42);

        assert_eq!(subject.set_u16(0, 24), Err(String::from("Index out of bounds: 0")));
    }
}