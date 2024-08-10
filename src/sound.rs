pub mod snd_cpal;

pub mod snd_base {
    use blip_buf::BlipBuf;

    pub struct Snd<T> {
        pub dev: T,
        pub sample_rate: u32,
        pub channels: u8,
        pub volume_gain: u16,
    }

    pub trait SndFunc {
        fn new() -> Self;
        fn init(&mut self);
        /* sample_buf value is 0~10000 */
        fn play(&mut self, sample_buf: &mut BlipBuf);
    }
}
