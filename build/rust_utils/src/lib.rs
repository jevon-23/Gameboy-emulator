#[repr(C)]
pub struct RV16 {
    rv : u16,
    over_flow : bool,
}
impl RV16 {
    pub fn new(rv : u16, over_flow : bool) -> Self {
        return Self{
            rv : rv,
            over_flow : over_flow,
        }
    }
}

#[repr(C)]
pub struct RV8 {
    rv : u8,
    over_flow : bool,
}

impl RV8 {
    pub fn new(rv : u8, over_flow : bool) -> Self {
        return Self{
            rv : rv,
            over_flow : over_flow,
        }
    }
}

#[no_mangle]
pub extern "C" fn add_overflow8(arg1 : u8, arg2 : u8) -> RV8 {
    let add_result : (u8, bool) = arg1.overflowing_add(arg2);
    return RV8::new(add_result.0, add_result.1);
}

#[no_mangle]
pub extern "C" fn add_overflow16(arg1 : u16, arg2 : u16) -> RV16 {
    let add_result : (u16, bool) = arg1.overflowing_add(arg2);

    return RV16::new(add_result.0, add_result.1);
}
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
