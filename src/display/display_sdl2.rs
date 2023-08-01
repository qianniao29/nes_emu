pub mod disp_sdl2 {
    //sdl
    use crate::display::disp::{Display, DisplayFunc};
    use sdl2::event::Event;
    use sdl2::keyboard::Keycode;
    use sdl2::pixels::{Color, PixelFormatEnum};
    use sdl2::rect::{Point, Rect};
    #[cfg(feature = "unsafe_textures")]
    use sdl2::render::Texture;

    use std::cell::RefCell;
    use std::rc::Rc;
    use std::time::Duration;

    pub struct DispSDL2 {
        pub sdl_context: sdl2::Sdl,
        pub canvas: sdl2::render::Canvas<sdl2::video::Window>,
        pub texture: sdl2::render::Texture,
    }

    impl DispSDL2 {
        pub fn new() -> Display<DispSDL2, [u8; 3]> {
            let sdl_context = sdl2::init().unwrap();
            let video_subsystem = sdl_context.video().unwrap();

            let window = video_subsystem
                .window("rust-nes-emulator", 256, 240)
                .position_centered()
                .build()
                .unwrap();

            let mut canvas = window.into_canvas().build().unwrap();

            let texture_creator = canvas.texture_creator();

            let texture = texture_creator
                .create_texture_streaming(PixelFormatEnum::RGB24, 8, 8)
                .unwrap();

            let palette_data: [[u8; 3]; 16] = [[0; 3]; 16];
            let mut tile_color_indx: [Vec<u8>; 8] = Default::default();
            for v in &mut tile_color_indx {
                v.resize(16, 0);
            }

            canvas.set_draw_color(Color::RGB(10, 10, 10));
            canvas.clear();
            canvas.present();

            Display {
                dev: DispSDL2 {
                    sdl_context,
                    canvas,
                    texture,
                },
                palette_data,
                tile_color_indx,
            }
        }
    }

    impl DisplayFunc<DispSDL2, [u8; 3]> for Display<DispSDL2, [u8; 3]> {
        fn generate_palette_data(&mut self, palette_indx_tbl: &[u8; 0x20]) {
            //genert platette data
            for i in 0..16 {
                let a = palette_indx_tbl[i];
                self.palette_data[i] = PLALETTE_STD_DATA[a as usize];
            }
            self.palette_data[4 * 1] = self.palette_data[0];
            self.palette_data[4 * 2] = self.palette_data[0];
            self.palette_data[4 * 3] = self.palette_data[0];
        }
        fn draw_tile(&mut self, x: u16, y: u16) {
            self.dev
                .texture
                .with_lock(None, |buffer: &mut [u8], pitch: usize| {
                    for j in 0..8 {
                        for i in 0..8 {
                            let offset = j * pitch + i * 3;
                            let color_indx = self.tile_color_indx[j][i] as usize;
                            buffer[offset] = self.palette_data[color_indx][0];
                            buffer[offset + 1] = self.palette_data[color_indx][1];
                            buffer[offset + 2] = self.palette_data[color_indx][2];
                        }
                    }
                })
                .unwrap();
            self.dev
                .canvas
                .copy(&self.dev.texture, None, Rect::new(x.into(), y.into(), 8, 8))
                .unwrap();
        }
        fn display_present(&mut self) {
            self.dev.canvas.present();
        }
    }

    #[rustfmt::skip]
    pub const PLALETTE_STD_DATA: [[u8; 3]; 64] = [
        [0x7F, 0x7F, 0x7F], [0x20, 0x00, 0xB0], [0x28, 0x00, 0xB8], [0x60, 0x10, 0xA0],
        [0x98, 0x20, 0x78], [0xB0, 0x10, 0x30], [0xA0, 0x30, 0x00], [0x78, 0x40, 0x00],
        [0x48, 0x58, 0x00], [0x38, 0x68, 0x00], [0x38, 0x6C, 0x00], [0x30, 0x60, 0x40],
        [0x30, 0x50, 0x80], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00],
        [0xBC, 0xBC, 0xBC], [0x40, 0x60, 0xF8], [0x40, 0x40, 0xFF], [0x90, 0x40, 0xF0],
        [0xD8, 0x40, 0xC0], [0xD8, 0x40, 0x60], [0xE0, 0x50, 0x00], [0xC0, 0x70, 0x00],
        [0x88, 0x88, 0x00], [0x50, 0xA0, 0x00], [0x48, 0xA8, 0x10], [0x48, 0xA0, 0x68],
        [0x40, 0x90, 0xC0], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00],
        [0xFF, 0xFF, 0xFF], [0x60, 0xA0, 0xFF], [0x50, 0x80, 0xFF], [0xA0, 0x70, 0xFF],
        [0xF0, 0x60, 0xFF], [0xFF, 0x60, 0xB0], [0xFF, 0x78, 0x30], [0xFF, 0xA0, 0x00],
        [0xE8, 0xD0, 0x20], [0x98, 0xE8, 0x00], [0x70, 0xF0, 0x40], [0x70, 0xE0, 0x90],
        [0x60, 0xD0, 0xE0], [0x60, 0x60, 0x60], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00],
        [0xFF, 0xFF, 0xFF], [0x90, 0xD0, 0xFF], [0xA0, 0xB8, 0xFF], [0xC0, 0xB0, 0xFF],
        [0xE0, 0xB0, 0xFF], [0xFF, 0xB8, 0xE8], [0xFF, 0xC8, 0xB8], [0xFF, 0xD8, 0xA0],
        [0xFF, 0xF0, 0x90], [0xC8, 0xF0, 0x80], [0xA0, 0xF0, 0xA0], [0xA0, 0xFF, 0xC8],
        [0xA0, 0xFF, 0xF0], [0xA0, 0xA0, 0xA0], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00],
    ];
}
