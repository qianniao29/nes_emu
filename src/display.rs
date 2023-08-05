pub mod display_sdl2;

pub mod disp {
    use crate::cpu2a03::cpu;

    pub struct Display<T, M> {
        pub dev: T,
        pub palette_data: [M; 16],
        pub tile_color_indx: [Vec<u8>; 8],
    }

    pub trait DisplayFunc<T, M> {
        fn generate_palette_data(&mut self, palette_indx_tbl: &[u8]);
        fn draw_tile(&mut self, x: u16, y: u16);
        fn draw_sprite(&mut self, x: u16, y: u16);
        fn display_present(&mut self);
    }

    pub fn vblank(cpu_reg: &mut cpu::Register, mem: &mut cpu::MemMap) {
        mem.ppu_reg.status.set_v(true);
        if mem.ppu_reg.ctrl.v() {
            cpu::nmi_handler(cpu_reg, mem);
        }
    }
}
