pub mod display_sdl2;

pub mod disp {
    use crate::cpu2a03::cpu;

    pub struct Display<T, M> {
        pub dev: T,
        pub palette_data: [M; 16],
        pub scanline_color_indx: [u8; 256],
    }

    pub struct DisStandard {
        pub cpu_clock_hz: u32,
        pub master_cycles_scanline: u16,
        pub cpu_cycle_per_frame: u16,
        pub master_cycles_per_cpu: u8,
        pub frame_rate: u8,
        pub vblank_length: u8,
    }

    pub const NTSC: DisStandard = DisStandard {
        cpu_clock_hz: 1789773,
        master_cycles_scanline: 1364,
        cpu_cycle_per_frame: 29780,
        master_cycles_per_cpu: 12,
        frame_rate: 60,
        vblank_length: 20,
    };

    pub trait DisplayFunc<T, M> {
        fn generate_palette_data(&mut self, palette_indx_tbl: &[u8]);
        fn draw_bg_tile(&mut self, x: u16, y: u16);
        fn draw_bg_scanline(&mut self, x: u16, y: u16);
        fn draw_sprite(&mut self, x: u16, y: u16);
        fn draw_sprite_scanline(&mut self, x: u16, y: u16);
        fn display_present(&mut self);
    }

    pub fn vblank(cpu_reg: &mut cpu::Register, mem: &mut cpu::MemMap) {
        mem.ppu_reg.status.set_v(true);
        if mem.ppu_reg.ctrl.v() {
            cpu::nmi_handler(cpu_reg, mem);
        }
    }
}
