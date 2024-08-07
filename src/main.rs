#![allow(dead_code)]
#![allow(unused_imports)]

#[macro_use]
extern crate bitfield;

mod common;
mod cpu2a03;
mod display;
mod input;
mod ppu2c02;
mod rom_fs;
mod sound;

use std::{
    cell::RefCell,
    env,
    ptr::addr_of_mut,
    rc::Rc,
    time::{Duration, Instant},
};

use common::{error, Bus, Irq};
use cpu2a03::{apu, cpu, cycle::cpu_cycles_reset, memory};
use display::disp::{self, DisplayFunc};
use display::display_sdl2::disp_sdl2;
use input::input::InputFunc;
use input::input_sdl2::input_sdl2;
use ppu2c02::ppu;
use rom_fs::rom::{self, RomHead};
use sound::{
    snd_base::{self, SndFunc},
    snd_cpal::snd_cpal,
};

struct DummyBus;
impl Bus for DummyBus {}
impl Irq for DummyBus {}

static mut DUMMY: DummyBus = DummyBus {};

struct Soc {
    pub cpu: cpu::Core,
    pub ppu: ppu::Ppu,
    pub apu: apu::Apu,
    pub mem: memory::MemMap,
    pub rom_head: RomHead,
}

impl Soc {
    pub fn new(sample_rate: u32) -> Self {
        Self {
            cpu: cpu::Core::new(unsafe { addr_of_mut!(DUMMY) }),
            ppu: ppu::Ppu::new(unsafe { addr_of_mut!(DUMMY) }),
            apu: apu::Apu::new(
                disp::NTSC.cpu_clock_hz,
                sample_rate,
                disp::TV_SYSTEM_NTSC,
                unsafe { addr_of_mut!(DUMMY) },
                unsafe { addr_of_mut!(DUMMY) },
            ),
            mem: memory::MemMap::new(unsafe { addr_of_mut!(DUMMY) }, unsafe {
                addr_of_mut!(DUMMY)
            }),
            rom_head: Default::default(),
        }
    }

    pub fn init(&mut self, rom_file_name: &String) {
        /*----------------------------------ROM init---------------------------------------*/
        let (prg_buf, pattern_buff1k) = match self.rom_head.load_rom(rom_file_name) {
            Ok((prg, pat)) => (prg, pat),
            Err(error) => {
                panic!("Problem opening the file: {:?}", error)
            }
        };
        let prgrom_buf = match prg_buf {
            Some(v) => v,
            None => panic!("load_rom error!"),
        };
        // println!("head:{:#?}", self.rom_head);
        assert_eq!(
            self.rom_head.timing,
            disp::TV_SYSTEM_NTSC,
            "Just support NTSC system!!!"
        );
        /*-------------------------------------^---------------------------------------------*/

        /*------------------------------CPU memory init--------------------------------------*/
        let mut offset = 0;
        if self.rom_head.prgrom_size_16k > 1 {
            offset = 0x4000;
        }
        self.mem.prg_rom = [
            &prgrom_buf[0x0..0x2000],
            &prgrom_buf[0x2000..0x4000],
            &prgrom_buf[offset + 0x00..offset + 0x2000],
            &prgrom_buf[offset + 0x2000..offset + 0x4000],
        ];
        self.mem.bus_to_apu = &mut self.apu;
        self.mem.bus_to_ppu = &mut self.ppu;
        /*-------------------------------------^----------------------------------------------*/

        /*------------------------------CPU Register init-------------------------------------*/
        self.cpu.bus_to_cpu_mem = &mut self.mem;
        self.cpu.reset();
        /*-------------------------------------^----------------------------------------------*/

        /*--------------------------------PPU init--------------------------------------------*/
        for i in 0..8 {
            self.ppu.mem.bank[i] = pattern_buff1k[i].clone();
        }
        self.ppu.mem.mapping_name_table(
            self.rom_head.flag6.four_screen(),
            self.rom_head.flag6.mirror_flag(),
        );
        self.ppu.irq = &mut self.cpu;
        /*-----------------------------------^------------------------------------------------*/

        /*--------------------------------APU init--------------------------------------------*/
        self.apu.irq = &mut self.cpu;
        self.apu.bus_to_cpu_mem = &mut self.mem;
        /*-----------------------------------^------------------------------------------------*/
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_name = &args[1];
    println!("File name: {}.", file_name);

    /*------------------------------Display init------------------------------------------*/
    let dis_std = disp::NTSC;
    let mut disp = disp_sdl2::DispSDL2::new();
    /*-----------------------------------^------------------------------------------------*/

    /*------------------------------Input init--------------------------------------------*/
    let mut input = input_sdl2::InputSDL2::new(&disp.dev.sdl_context);
    /*-----------------------------------^------------------------------------------------*/

    /*------------------------------Sound init--------------------------------------------*/
    let mut sound = snd_base::Snd::new();
    sound.init();
    /*----------------------------------^-------------------------------------------------*/

    /*-------------------------------SOC init---------------------------------------------*/
    let mut soc = Soc::new(sound.sample_rate);
    soc.init(file_name);
    /*----------------------------------^-------------------------------------------------*/

    let mut cpu_cycles_end;
    let mut master_cycles: u32;
    let mut sprite0_check_buf = [0_u8; 16];
    let mut is_odd_frame = false;
    let mut start;
    // let mut trig_apu_timers = |_: &mut cpu::Register, _: &mut memory::MemMap, ntick: u32| {
    //     soc.apu.trig_timers(ntick);
    // };
    'running: loop {
        start = Instant::now();
        master_cycles = 0;
        cpu_cycles_reset();
        let (mut sprite0_x, mut sprite0_y) = (0xff, 0xff);
        let mut sprite0_should_hit;

        if soc.ppu.reg.mask.s() && soc.ppu.reg.mask.bg() {
            sprite0_check_buf = [0_u8; 16];
            (sprite0_x, sprite0_y) = soc.ppu.get_sprint0(&mut sprite0_check_buf);
        }

        /*---------------------------------Visible scanlines---------------------------------*/
        for j in 0..240 {
            if soc.ppu.reg.mask.bg() {
                // render background
                soc.ppu.render_bg_scanline(&mut disp.scanline_color_indx);
                //genert bg platette data
                disp.generate_palette_data(&soc.ppu.mem.palette_indx_tbl[0..16]);
                disp.draw_bg_scanline(if soc.ppu.reg.mask.bm() { 0 } else { 8 }, j);
            }

            // execute code in one scanline cycles
            master_cycles += dis_std.master_cycles_scanline as u32;
            cpu_cycles_end = (master_cycles / dis_std.master_cycles_per_cpu as u32) as u16;
            soc.cpu.execute_instruction_until_and_hook(
                &mut soc.mem,
                cpu_cycles_end,
                |_, _, ntick: u16| {
                    // APU trigger frame counter
                    soc.apu.frame_counter_trig(ntick);
                },
            );
            // let snapshot_cyc = cpu::get_cpu_cycles();
            // while cpu::get_cpu_cycles() <= cpu_cycles_end {
            //     cpu.execute_one_instruction(&mut mem);
            //     // sprite0 hit
            //     if sprite0_should_hit {
            //         let cyc_indx = cpu::get_cpu_cycles() - snapshot_cyc;
            //         if !(mem.ppu_reg.mask.bm() | mem.ppu_reg.mask.sm()) {
            //             if (cyc_indx >= 9 / 3)
            //                 && (cyc_indx >= sprite0_x as u32 / 3)
            //                 && (cyc_indx < 258 / 3)
            //             {
            //                 mem.ppu_reg.status.set_s(true);
            //             }
            //         } else if (cyc_indx >= sprite0_x as u32 / 3) && (cyc_indx < 258 / 3) {
            //             mem.ppu_reg.status.set_s(true);
            //         }
            //     }
            // }

            /* all 262 line trigger 4 times, so triggering per 65 line. */
            if j == 65 || j == 130 || j == 195 {
                soc.apu.mix(&mut sound.dev.buffer.lock().unwrap());
            }

            //dot 256, 257
            if soc.ppu.reg.mask.bg() {
                soc.ppu.coarse_y_wrapping();
                soc.ppu.cpoy_x_from_t_to_v();
            }
            //sync horizon

            //check sprite0 hiting
            sprite0_should_hit = false;
            if soc.ppu.reg.mask.bg() {
                sprite0_should_hit = soc.ppu.check_sprint0_hit(
                    j,
                    sprite0_x,
                    sprite0_y,
                    &sprite0_check_buf,
                    &disp.scanline_color_indx,
                );
            }
            if sprite0_should_hit {
                soc.ppu.reg.status.set_s(true);
            }

            //render sprite
            if soc.ppu.reg.mask.bg() || soc.ppu.reg.mask.s() {
                soc.ppu
                    .render_sprite_scanline(j, &mut disp.scanline_color_indx);
            }

            if soc.ppu.reg.mask.s() {
                //genert sprite platette data
                disp.generate_palette_data(&soc.ppu.mem.palette_indx_tbl[16..32]);
                //actually, sprite y is + 1
                disp.draw_sprite_scanline(if soc.ppu.reg.mask.sm() { 0 } else { 8 }, j);
            }
        }
        /*----------------------------------^-------------------------------------------*/

        /*------------------------------Post-render scanline----------------------------*/
        master_cycles += dis_std.master_cycles_scanline as u32;
        cpu_cycles_end = (master_cycles / dis_std.master_cycles_per_cpu as u32) as u16;
        soc.cpu.execute_instruction_until_and_hook(
            &mut soc.mem,
            cpu_cycles_end,
            |_, _, ntick: u16| {
                // APU trigger frame counter
                soc.apu.frame_counter_trig(ntick);
            },
        );
        //sync horizon
        /*----------------------------------^-------------------------------------------*/

        /*-------------------------------Vblank scanline--------------------------------*/
        soc.ppu.vblank();
        // execute code after NMI
        for _ in 0..dis_std.vblank_length {
            master_cycles += dis_std.master_cycles_scanline as u32;
            cpu_cycles_end = (master_cycles / dis_std.master_cycles_per_cpu as u32) as u16;
            soc.cpu.execute_instruction_until_and_hook(
                &mut soc.mem,
                cpu_cycles_end,
                |_, _, ntick: u16| {
                    // APU trigger frame counter
                    soc.apu.frame_counter_trig(ntick);
                },
            );
            //sync horizon
        }
        /*----------------------------------^------------------------------------------*/

        /*----------------------------Pre-render scanlines-----------------------------*/
        //clear ppu status, vblank
        soc.ppu.reg.status.0 = 0;

        soc.cpu.execute_instruction_until_and_hook(
            &mut soc.mem,
            dis_std.cpu_cycle_per_frame - if is_odd_frame { 3 } else { 0 },
            |_, _, ntick: u16| {
                // APU trigger frame counter
                soc.apu.frame_counter_trig(ntick);
            },
        );
        is_odd_frame = !is_odd_frame;
        //sync horizon

        //APU trigger 4th frame counter
        soc.apu.mix(&mut sound.dev.buffer.lock().unwrap());

        if soc.ppu.reg.mask.bg() {
            soc.ppu.cpoy_y_from_t_to_v();
        }
        /*----------------------------------^------------------------------------------*/

        /*------------------------------Display screen---------------------------------*/
        disp.display_present();
        /*----------------------------------^------------------------------------------*/

        /*--------------------------------Scan input-----------------------------------*/
        if input.get_key(&mut soc.mem.key) == usize::MAX {
            break 'running;
        }
        /*----------------------------------^------------------------------------------*/

        /*------------------------Sleep & Control frame rate---------------------------*/
        let sleep_usec = Duration::from_micros(1_000_000 / dis_std.frame_rate as u64)
            .saturating_sub(start.elapsed())
            + Duration::from_micros(1); //add 1 in case sleep_usec is 0.

        // println!(
        //     "Time elapsed: {:?}, sleep {:?}",
        //     start.elapsed(),
        //     sleep_usec
        // );
        std::thread::sleep(sleep_usec);
        /*----------------------------------^------------------------------------------*/
    }
}
