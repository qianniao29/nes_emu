pub mod input_sdl2 {
    use crate::cpu2a03::cpu;
    use crate::input::input::{Input, InputFunc};
    use sdl2::event::Event;
    use sdl2::keyboard::Keycode;

    pub struct InputSDL2 {
        pub event_pump: sdl2::EventPump,
    }

    impl InputSDL2 {
        pub fn new(sdl_context: &sdl2::Sdl) -> Input<InputSDL2> {
            Input {
                dev: InputSDL2 {
                    event_pump: sdl_context.event_pump().unwrap(),
                },
            }
        }
    }
    impl InputFunc for Input<InputSDL2> {
        fn get_key(&mut self, key: &mut [[u8; 8]; 2]) -> usize {
            for event in self.dev.event_pump.poll_iter() {
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => return usize::MAX,
                    Event::KeyDown {
                        keycode: Some(Keycode::A),
                        ..
                    } => {
                        key[0][cpu::KEY_LEFT] = 1;
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::D),
                        ..
                    } => {
                        key[0][cpu::KEY_RIGHT] = 1;
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::W),
                        ..
                    } => {
                        key[0][cpu::KEY_UP] = 1;
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::S),
                        ..
                    } => {
                        key[0][cpu::KEY_DOWN] = 1;
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::U),
                        ..
                    } => {
                        key[0][cpu::KEY_SELECT] = 1;
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::I),
                        ..
                    } => {
                        key[0][cpu::KEY_START] = 1;
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::J),
                        ..
                    } => {
                        key[0][cpu::KEY_A] = 1;
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::K),
                        ..
                    } => {
                        key[0][cpu::KEY_B] = 1;
                    }
                    Event::KeyUp {
                        keycode: Some(Keycode::A),
                        ..
                    } => {
                        key[0][cpu::KEY_LEFT] = 0;
                    }
                    Event::KeyUp {
                        keycode: Some(Keycode::D),
                        ..
                    } => {
                        key[0][cpu::KEY_RIGHT] = 0;
                    }
                    Event::KeyUp {
                        keycode: Some(Keycode::W),
                        ..
                    } => {
                        key[0][cpu::KEY_UP] = 0;
                    }
                    Event::KeyUp {
                        keycode: Some(Keycode::S),
                        ..
                    } => {
                        key[0][cpu::KEY_DOWN] = 0;
                    }
                    Event::KeyUp {
                        keycode: Some(Keycode::U),
                        ..
                    } => {
                        key[0][cpu::KEY_SELECT] = 0;
                    }
                    Event::KeyUp {
                        keycode: Some(Keycode::I),
                        ..
                    } => {
                        key[0][cpu::KEY_START] = 0;
                    }
                    Event::KeyUp {
                        keycode: Some(Keycode::J),
                        ..
                    } => {
                        key[0][cpu::KEY_A] = 0;
                    }
                    Event::KeyUp {
                        keycode: Some(Keycode::K),
                        ..
                    } => {
                        key[0][cpu::KEY_B] = 0;
                    }
                    _ => {}
                }
            }
            0
        }
    }
}
