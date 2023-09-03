pub mod disp_sdl2 {
    //sdl
    use crate::display::disp::{Display, DisplayFunc};
    use sdl2::event::Event;
    use sdl2::keyboard::Keycode;
    use sdl2::pixels::{Color, PixelFormatEnum};
    use sdl2::rect::{Point, Rect};
    use sdl2::render::{Canvas, Texture};

    use std::cell::RefCell;
    use std::rc::Rc;
    use std::time::Duration;

    pub struct DispSDL2 {
        pub sdl_context: sdl2::Sdl,
        pub canvas: Canvas<sdl2::video::Window>,
        pub texture: Texture,
    }

    impl DispSDL2 {
        pub fn new() -> Display<DispSDL2, [u8; 4]> {
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
                .create_texture_streaming(PixelFormatEnum::ARGB32, 256, 1)
                .unwrap();

            let palette_data: [[u8; 4]; 16] = [[0; 4]; 16];
            let scanline_color_indx = [0; 256];

            canvas.set_draw_color(Color::RGB(0, 0, 0));
            canvas.clear();
            canvas.present();

            Display {
                dev: DispSDL2 {
                    sdl_context,
                    canvas,
                    texture,
                },
                palette_data,
                scanline_color_indx,
            }
        }
    }

    impl DisplayFunc<DispSDL2, [u8; 4]> for Display<DispSDL2, [u8; 4]> {
        fn generate_palette_data(&mut self, palette_indx_tbl: &[u8]) {
            //genert platette data
            for i in 0..16 {
                self.palette_data[i] = PLALETTE_STD_DATA[palette_indx_tbl[i] as usize];
            }
        }
        fn draw_bg_tile(&mut self, x: u16, y: u16) {
            self.dev
                .texture
                .with_lock(None, |buffer: &mut [u8], pitch: usize| {
                    for j in 0..8 {
                        let tile_color_indx = &self.scanline_color_indx[(j * 8)..(j * 8 + 8)];
                        for i in 0..8 {
                            let offset = j * pitch + i * 4;
                            let color_indx = tile_color_indx[i] as usize;
                            buffer[offset..offset + 4]
                                .clone_from_slice(&self.palette_data[color_indx][..]);
                        }
                    }
                })
                .unwrap();
            self.dev
                .canvas
                .copy(&self.dev.texture, None, Rect::new(x.into(), y.into(), 8, 8))
                .unwrap();
        }
        fn draw_bg_scanline(&mut self, x: u16, y: u16) {
            self.dev
                .texture
                .with_lock(None, |buffer: &mut [u8], _| {
                    for i in x as usize..256 {
                        let offset = i * 4;
                        let color_indx = self.scanline_color_indx[i] as usize;
                        buffer[offset..offset + 4]
                            .clone_from_slice(&self.palette_data[color_indx][..]);
                    }
                })
                .unwrap();
            self.dev
                .canvas
                .copy(
                    &self.dev.texture,
                    None,
                    Rect::new(x.into(), y.into(), 256 - x as u32, 1),
                )
                .unwrap();
        }
        fn draw_sprite(&mut self, x: u16, y: u16) {
            for j in 0..8 {
                let tile_color_indx = &self.scanline_color_indx[(j * 8)..(j * 8 + 8)];
                for i in 0..8 {
                    let color_indx = tile_color_indx[i] as usize;
                    if color_indx & 3 == 0 {
                        continue;
                    } // 透明色精灵，保持背景色不变
                    self.dev.canvas.set_draw_color(Color::RGBA(
                        self.palette_data[color_indx][1],
                        self.palette_data[color_indx][2],
                        self.palette_data[color_indx][3],
                        self.palette_data[color_indx][0],
                    ));
                    self.dev
                        .canvas
                        .draw_point((x as i32 + i as i32, y as i32 + j as i32))
                        .unwrap();
                }
            }
        }
        fn draw_sprite_scanline(&mut self, x: u16, y: u16) {
            for i in x as usize..256 {
                let color_indx = self.scanline_color_indx[i] as usize;
                if color_indx & 3 == 0 {
                    continue;
                } // 透明色精灵，保持背景色不变
                self.dev.canvas.set_draw_color(Color::RGBA(
                    self.palette_data[color_indx][1],
                    self.palette_data[color_indx][2],
                    self.palette_data[color_indx][3],
                    self.palette_data[color_indx][0],
                ));
                self.dev.canvas.draw_point((i as i32, y as i32)).unwrap();
            }
        }
        fn display_present(&mut self) {
            self.dev.canvas.present();
        }
    }

    #[rustfmt::skip]
    const PLALETTE_STD_DATA: [[u8; 4]; 64] = [
        [0xFF, 0x7F, 0x7F, 0x7F], [0xFF, 0x20, 0x00, 0xB0], [0xFF, 0x28, 0x00, 0xB8], [0xFF, 0x60, 0x10, 0xA0],
        [0xFF, 0x98, 0x20, 0x78], [0xFF, 0xB0, 0x10, 0x30], [0xFF, 0xA0, 0x30, 0x00], [0xFF, 0x78, 0x40, 0x00],
        [0xFF, 0x48, 0x58, 0x00], [0xFF, 0x38, 0x68, 0x00], [0xFF, 0x38, 0x6C, 0x00], [0xFF, 0x30, 0x60, 0x40],
        [0xFF, 0x30, 0x50, 0x80], [0xFF, 0x00, 0x00, 0x00], [0xFF, 0x00, 0x00, 0x00], [0xFF, 0x00, 0x00, 0x00],
        [0xFF, 0xBC, 0xBC, 0xBC], [0xFF, 0x40, 0x60, 0xF8], [0xFF, 0x40, 0x40, 0xFF], [0xFF, 0x90, 0x40, 0xF0],
        [0xFF, 0xD8, 0x40, 0xC0], [0xFF, 0xD8, 0x40, 0x60], [0xFF, 0xE0, 0x50, 0x00], [0xFF, 0xC0, 0x70, 0x00],
        [0xFF, 0x88, 0x88, 0x00], [0xFF, 0x50, 0xA0, 0x00], [0xFF, 0x48, 0xA8, 0x10], [0xFF, 0x48, 0xA0, 0x68],
        [0xFF, 0x40, 0x90, 0xC0], [0xFF, 0x00, 0x00, 0x00], [0xFF, 0x00, 0x00, 0x00], [0xFF, 0x00, 0x00, 0x00],
        [0xFF, 0xFF, 0xFF, 0xFF], [0xFF, 0x60, 0xA0, 0xFF], [0xFF, 0x50, 0x80, 0xFF], [0xFF, 0xA0, 0x70, 0xFF],
        [0xFF, 0xF0, 0x60, 0xFF], [0xFF, 0xFF, 0x60, 0xB0], [0xFF, 0xFF, 0x78, 0x30], [0xFF, 0xFF, 0xA0, 0x00],
        [0xFF, 0xE8, 0xD0, 0x20], [0xFF, 0x98, 0xE8, 0x00], [0xFF, 0x70, 0xF0, 0x40], [0xFF, 0x70, 0xE0, 0x90],
        [0xFF, 0x60, 0xD0, 0xE0], [0xFF, 0x60, 0x60, 0x60], [0xFF, 0x00, 0x00, 0x00], [0xFF, 0x00, 0x00, 0x00],
        [0xFF, 0xFF, 0xFF, 0xFF], [0xFF, 0x90, 0xD0, 0xFF], [0xFF, 0xA0, 0xB8, 0xFF], [0xFF, 0xC0, 0xB0, 0xFF],
        [0xFF, 0xE0, 0xB0, 0xFF], [0xFF, 0xFF, 0xB8, 0xE8], [0xFF, 0xFF, 0xC8, 0xB8], [0xFF, 0xFF, 0xD8, 0xA0],
        [0xFF, 0xFF, 0xF0, 0x90], [0xFF, 0xC8, 0xF0, 0x80], [0xFF, 0xA0, 0xF0, 0xA0], [0xFF, 0xA0, 0xFF, 0xC8],
        [0xFF, 0xA0, 0xFF, 0xF0], [0xFF, 0xA0, 0xA0, 0xA0], [0xFF, 0x00, 0x00, 0x00], [0xFF, 0x00, 0x00, 0x00],
    ];
}
