pub fn zero_vec_of_len(len: usize) -> Vec<u8> {
    assert!(len > 0);
    
    let mut result = Vec::new();
    for _ in 0..len {
        result.push(0u8);
    }
    result
}