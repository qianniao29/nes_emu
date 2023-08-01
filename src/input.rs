pub mod input_sdl2;

pub mod input {
    pub struct Input<T> {
        pub dev: T,
    }

    pub trait InputFunc {
        fn get_key(&mut self, key: &mut [[u8; 8]; 2]) -> usize;
    }
}
