#[no_mangle]
pub extern "C" fn test_fn(arg : u8) -> u8 {
    println!("Inside of rust test fn, received {}", arg);
    return 0x20;
}

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
