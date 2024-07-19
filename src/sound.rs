pub mod snd_cpal;

pub mod snd_base {
    pub struct Snd<T> {
        pub dev: T,
        pub sample_rate: u32,
    }

    pub trait SndFunc {
        fn new() -> Self;
        fn init(&mut self);
    }
}
