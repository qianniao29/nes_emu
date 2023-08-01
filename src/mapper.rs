pub mod mapper {

    pub struct Mapper {}

    pub trait MapperFunc {
        fn reset() -> i32;
    }
}
