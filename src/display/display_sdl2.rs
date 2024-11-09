pub mod disp_sdl2 {
    //sdl
    use crate::display::disp::{Display, DisplayFunc};
    use sdl2::pixels::{Color, PixelFormatEnum};
    use sdl2::rect::Rect;
    use sdl2::render::{Canvas, Texture};

    pub struct DispSDL2 {
        pub sdl_context: sdl2::Sdl,
        pub canvas: Canvas<sdl2::video::Window>,
        pub texture: Texture,
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
                .create_texture_streaming(PixelFormatEnum::RGB24, 256, 1)
                .unwrap();

            let palette_data: [[u8; 3]; 16] = [[0; 3]; 16];
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

    impl DisplayFunc<DispSDL2, [u8; 3]> for Display<DispSDL2, [u8; 3]> {
        fn generate_palette_data(&mut self, palette_indx_tbl: &[u8]) {
            for i in [1, 2, 3, 5, 6, 7, 9, 10, 11, 13, 14, 15] {
                self.palette_data[i] = PLALETTE_STD_DATA[palette_indx_tbl[i] as usize];
            }
            /*$3F04/$3F08/$3F0C values are not used by the PPU when normally rendering
            (since the pattern values that would otherwise select those cells select the backdrop color instead). */
            for i in [0, 4, 8, 12] {
                self.palette_data[i] = PLALETTE_STD_DATA[palette_indx_tbl[0] as usize];
            }
        }
        fn draw_bg_scanline(&mut self, x: u16, y: u16) {
            self.dev
                .texture
                .with_lock(None, |buffer: &mut [u8], _| {
                    for i in x as usize..256 {
                        let offset = i * 3;
                        let color_indx = self.scanline_color_indx[i] as usize;
                        buffer[offset..(offset + 3)]
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
        fn draw_sprite_scanline(&mut self, x: u16, y: u16) {
            for i in x as usize..256 {
                let color_indx = self.scanline_color_indx[i] as usize;
                if color_indx & 3 == 0 {
                    continue;
                } // 透明色精灵，保持背景色不变
                self.dev.canvas.set_draw_color(Color::RGB(
                    self.palette_data[color_indx][0],
                    self.palette_data[color_indx][1],
                    self.palette_data[color_indx][2],
                ));
                self.dev.canvas.draw_point((i as i32, y as i32)).unwrap();
            }
        }
        fn display_present(&mut self) {
            self.dev.canvas.present();
        }
    }

    #[rustfmt::skip]
    const PLALETTE_STD_DATA: [[u8; 3]; 64] = [
        [0x55, 0x55, 0x55], [0x00, 0x17, 0x73], [0x00, 0x07, 0x86], [0x2e, 0x05, 0x78],
        [0x59, 0x02, 0x4d], [0x72, 0x00, 0x11], [0x6e, 0x00, 0x00], [0x4c, 0x08, 0x00],
        [0x17, 0x1b, 0x00], [0x00, 0x2a, 0x00], [0x00, 0x31, 0x00], [0x00, 0x2e, 0x08],
        [0x00, 0x26, 0x45], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00],
        [0xa5, 0xa5, 0xa5], [0x00, 0x57, 0xc6], [0x22, 0x3f, 0xe5], [0x6e, 0x28, 0xd9],
        [0xae, 0x1a, 0xa6], [0xd2, 0x17, 0x59], [0xd1, 0x21, 0x07], [0xa7, 0x37, 0x00],
        [0x63, 0x51, 0x00], [0x18, 0x67, 0x00], [0x00, 0x72, 0x00], [0x00, 0x73, 0x31],
        [0x00, 0x6a, 0x84], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00],
        [0xfe, 0xff, 0xff], [0x2f, 0xa8, 0xff], [0x5d, 0x81, 0xff], [0x9c, 0x70, 0xff],
        [0xf7, 0x72, 0xff], [0xff, 0x77, 0xbd], [0xff, 0x7e, 0x75], [0xff, 0x8a, 0x2b],
        [0xcd, 0xa0, 0x00], [0x81, 0xb8, 0x02], [0x3d, 0xc8, 0x30], [0x12, 0xcd, 0x7b],
        [0x0d, 0xc5, 0xd0], [0x3c, 0x3c, 0x3c], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00],
        [0xfe, 0xff, 0xff], [0xa4, 0xde, 0xff], [0xb1, 0xc8, 0xff], [0xcc, 0xbe, 0xff],
        [0xf4, 0xc2, 0xff], [0xff, 0xc5, 0xea], [0xff, 0xc7, 0xc9], [0xff, 0xcd, 0xaa],
        [0xef, 0xd6, 0x96], [0xd0, 0xe0, 0x95], [0xb3, 0xe7, 0xa5], [0x9f, 0xea, 0xc3],
        [0x9a, 0xe8, 0xe6], [0xaf, 0xaf, 0xaf], [0x00, 0x00, 0x00], [0x00, 0x00, 0x00],
    ];
}
