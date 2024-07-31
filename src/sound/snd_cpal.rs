pub mod snd_cpal {
    use std::sync::{Arc, Mutex};

    use cpal::{
        traits::{DeviceTrait, HostTrait, StreamTrait},
        Sample, SampleFormat, SupportedStreamConfig,
    };

    use crate::sound::snd_base::{Snd, SndFunc};

    pub struct CpalDev {
        stream: cpal::Stream,
        pub buffer: Arc<Mutex<Vec<f32>>>,
    }

    impl CpalDev {
        fn write_silence<T: Sample>(data: &mut [T], _: &cpal::OutputCallbackInfo) {
            for sample in data.iter_mut() {
                *sample = Sample::EQUILIBRIUM;
            }
        }
    }

    impl SndFunc for Snd<CpalDev> {
        fn new() -> Self {
            let host = cpal::default_host();
            let device = host
                .default_output_device()
                .expect("no output device available");
            let def_config = device.default_output_config().unwrap();
            let sample_format = def_config.sample_format();
            let config: cpal::StreamConfig = def_config.into();
            let sample_rate = config.sample_rate.0;
            // println!(
            //     "channels:{},sample_format:{},buffer_size:{:?},rate:{}",
            //     config.channels, sample_format, config.buffer_size, sample_rate
            // );

            let err_fn = |err| eprintln!("an error occurred on the output audio stream: {}", err);

            let buf = Arc::new(Mutex::new(Vec::new()));
            let buffer = buf.clone();
            let stream = match sample_format {
                SampleFormat::F32 => device.build_output_stream(
                    &config,
                    move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                        let len = std::cmp::min(data.len() / 2, buffer.lock().unwrap().len());
                        // println!(
                        //     "paly f32 sound,len={},buffer len={}",
                        //     len,
                        //     buffer.lock().unwrap().len()
                        // );
                        for (i, v) in buffer.lock().unwrap().drain(..len).enumerate() {
                            data[i * 2 + 0] = v;
                            data[i * 2 + 1] = v;
                        }
                    },
                    err_fn,
                    None,
                ),
                SampleFormat::F64 => device.build_output_stream(
                    &config,
                    move |data: &mut [f64], _: &cpal::OutputCallbackInfo| {
                        let len = std::cmp::min(data.len() / 2, buffer.lock().unwrap().len());
                        // println!("paly f64 sound,len={}", len);
                        for (i, v) in buffer.lock().unwrap().drain(..len).enumerate() {
                            data[i * 2 + 0] = v.to_sample::<f64>();
                            data[i * 2 + 1] = v.to_sample::<f64>();
                        }
                    },
                    err_fn,
                    None,
                ),
                sample_format => panic!("Unsupported sample format '{sample_format}'"),
            }
            .unwrap();
            stream.play().unwrap();
            Snd {
                dev: CpalDev {
                    stream,
                    buffer: buf,
                },
                sample_rate,
            }
        }

        fn init(&mut self) {}
    }
}
