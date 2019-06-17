use crate::utils::zero_vec_of_len;

pub struct BinaryWriter {
    data: Vec<u8>,
    cursor: usize,
}

impl BinaryWriter {
    pub fn new(capacity: usize) -> Self {
        Self {
            data: zero_vec_of_len(capacity),
            cursor: 0,
        }
    }

    fn push(&mut self, value: u8) -> Result<(), String> {
        if self.cursor >= self.data.len() {
            Err("Attempt to write out of bounds".to_string())
        } else {
            self.data[self.cursor] = value;
            self.cursor += 1;
            Ok(())
        }
    }

    pub fn capacity(&self) -> usize {
        self.data.len()
    }

    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn set_cursor(&mut self, cursor: usize) -> Result<(), String> {
        if cursor >= self.data.len() {
            Err(format!("Cursor out of bounds: {}", cursor))
        } else {
            self.cursor = cursor;
            Ok(())
        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        self.data.as_slice()
    }

    pub fn put_u8(&mut self, value: u8) -> Result<usize, String> {
        self.push(value)?;
        Ok(self.cursor - 1)
    }

    pub fn put_u16(&mut self, value: u16) -> Result<usize, String> {
        let cursor = self.cursor;
        self.push((value & 0xFF) as u8)?;
        self.push(((value >> 8) & 0xFF) as u8)?;
        Ok(cursor)
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
    fn cursor_works() {
        let mut subject = BinaryWriter::new(100);

        subject.put_u8(42).unwrap();

        assert_eq!(subject.cursor(), 1);

        subject.put_u16(256).unwrap();

        assert_eq!(subject.cursor(), 3);
    }

    #[test]
    fn as_bytes_works() {
        let mut subject = BinaryWriter::new(100);

        subject.put_u8(42).unwrap();
        subject.put_u16(256).unwrap();

        let mut expected = zero_vec_of_len(100);
        expected[0] = 42;
        expected[1] = 0;
        expected[2] = 1;

        assert_eq!(subject.as_bytes(), expected.as_slice());
    }

    #[test]
    fn put_u8_works() {
        let mut subject = BinaryWriter::new(100);

        assert_eq!(subject.put_u8(42), Ok(0));
        assert_eq!(subject.put_u8(24), Ok(1));

        assert_eq!(subject.data[0], 42);
        assert_eq!(subject.data[1], 24);
    }

    #[test]
    fn put_u16_works() {
        let mut subject = BinaryWriter::new(100);

        assert_eq!(subject.put_u16(256), Ok(0));

        assert_eq!(subject.cursor(), 2);
        assert_eq!(subject.data[0], 0);
        assert_eq!(subject.data[1], 1);
    }

    #[test]
    fn set_u8_works() {
        let mut subject = BinaryWriter::new(100);

        assert_eq!(subject.put_u8(42), Ok(0));

        assert_eq!(subject.set_u8(0, 24), Ok(()));

        assert_eq!(subject.data[0], 24);
    }

    #[test]
    fn set_u8_returns_an_error_if_index_is_out_of_bounds() {
        let mut subject = BinaryWriter::new(5);

        assert_eq!(
            subject.set_u8(42, 24),
            Err(String::from("Index out of bounds: 42"))
        );
    }

    #[test]
    fn set_u16_works() {
        let mut subject = BinaryWriter::new(100);

        assert_eq!(subject.put_u16(256), Ok(0));

        assert_eq!(subject.set_u16(0, 24), Ok(()));

        assert_eq!(subject.data[0], 24);
        assert_eq!(subject.data[1], 0);
    }

    #[test]
    fn set_u16_returns_an_error_if_index_is_out_of_bounds() {
        let mut subject = BinaryWriter::new(1);

        assert_eq!(
            subject.set_u16(4, 24),
            Err(String::from("Index out of bounds: 4"))
        );
    }

    #[test]
    fn push_returns_err_if_out_of_bounds() {
        let mut subject = BinaryWriter::new(1);

        assert_eq!(subject.push(42), Ok(()));
        assert_eq!(
            subject.push(42),
            Err("Attempt to write out of bounds".to_string())
        );
    }
}
