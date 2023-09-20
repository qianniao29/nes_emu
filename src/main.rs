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

use ahash::AHashMap;
use std::cell::RefCell;
use std::env;
use std::rc::Rc;
use std::time::{Duration, Instant};

use crate::display::disp::{self, DisplayFunc};
use crate::display::display_sdl2::disp_sdl2;
use crate::input::input::InputFunc;
use crate::input::input_sdl2::input_sdl2;
use common::error;
use cpu2a03::cpu;
use ppu2c02::ppu;
use rom_fs::rom;

fn reset(cpu_reg: &mut cpu::Register, mem: &mut cpu::MemMap) {
    cpu_reg.reset(mem);
}

fn main() -> Result<(), error::CustomError> {
    let args: Vec<String> = env::args().collect();
    let file_name = &args[1];
    println!("File name: {}.", file_name);

    let (head, prgrom_buf, pattern_buff1k) = rom::load_rom(file_name)?;
    // println!("head:{:#?}",head);
    let mut mem = cpu::MemMap::new();
    let mut offset = 0;
    if head.prgrom_size_16k > 1 {
        offset = 0x4000;
    }
    mem.prg_rom = [
        &prgrom_buf[0x0..0x2000],
        &prgrom_buf[0x2000..0x4000],
        &prgrom_buf[offset + 0x00..offset + 0x2000],
        &prgrom_buf[offset + 0x2000..offset + 0x4000],
    ];

    for i in 0..8 {
        mem.ppu_mem.bank[i] = pattern_buff1k[i].clone();
    }

    mem.ppu_mem
        .mapping_name_table(head.flag6.four_screen(), head.flag6.mirror_flag());

    let mut cpu_reg = cpu::Register::new();
    reset(&mut cpu_reg, &mut mem);

    /*------------------------------display init------------------------------------------*/
    let mut disp = disp_sdl2::DispSDL2::new();
    let dis_std = disp::NTSC;
    /*------------------------------------------------------------------------------------*/
    /*------------------------------input init--------------------------------------------*/
    let mut input = input_sdl2::InputSDL2::new(&disp.dev.sdl_context);
    /*------------------------------------------------------------------------------------*/

    let mut cpu_cycles_end;
    let mut master_cycles: u32;
    let mut sprite0_check_buf = [0_u8; 16];
    let mut is_odd_frame = false;
    'running: loop {
        let start = Instant::now();
        master_cycles = 0;
        cpu::cpu_cycles_reset();
        let (mut sprite0_x, mut sprite0_y) = (0xff, 0xff);
        let mut sprite0_should_hit;

        if mem.ppu_reg.mask.s() && mem.ppu_reg.mask.bg() {
            sprite0_check_buf = [0_u8; 16];
            (sprite0_x, sprite0_y) =
                ppu::get_sprint0(&mem.ppu_reg, &mut mem.ppu_mem, &mut sprite0_check_buf);
        }

        /*------------Visible scanlines------------*/
        for j in 0..240 {
            if mem.ppu_reg.mask.bg() {
                // render background
                ppu::render_bg_scanline(
                    &mut mem.ppu_reg,
                    &mut mem.ppu_mem,
                    &mut disp.scanline_color_indx,
                );
                //genert bg platette data
                disp.generate_palette_data(&mem.ppu_mem.palette_indx_tbl[0..16]);
                disp.draw_bg_scanline(if mem.ppu_reg.mask.bm() { 0 } else { 8 }, j);
            }

            // execute code in one scanline cycles
            master_cycles += dis_std.master_cycles_scanline as u32;
            cpu_cycles_end = master_cycles / dis_std.master_cycles_per_cpu as u32;
            cpu::execute_instruction_until(&mut cpu_reg, &mut mem, cpu_cycles_end);
            // let snapshot_cyc = cpu::get_cpu_cycles();
            // while cpu::get_cpu_cycles() <= cpu_cycles_end {
            //     cpu::execute_one_instruction(&mut cpu_reg, &mut mem);
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

            //dot 256, 257
            if mem.ppu_reg.mask.bg() {
                ppu::coarse_y_wrapping(&mut mem.ppu_reg);
                ppu::cpoy_x_from_t_to_v(&mut mem.ppu_reg);
            }
            //sync horizon

            //check sprite0 hiting
            sprite0_should_hit = false;
            if mem.ppu_reg.mask.bg() {
                sprite0_should_hit = ppu::check_sprint0_hit(
                    j,
                    sprite0_x,
                    sprite0_y,
                    &mut mem.ppu_reg,
                    &sprite0_check_buf,
                    &disp.scanline_color_indx,
                );
            }
            if sprite0_should_hit {
                mem.ppu_reg.status.set_s(true);
            }

            //render sprite
            if mem.ppu_reg.mask.bg() || mem.ppu_reg.mask.s() {
                ppu::render_sprite_scanline(
                    j,
                    &mut mem.ppu_reg,
                    &mut mem.ppu_mem,
                    &mut disp.scanline_color_indx,
                );
            }

            if mem.ppu_reg.mask.s() {
                //genert bg platette data
                disp.generate_palette_data(&mem.ppu_mem.palette_indx_tbl[16..32]);
                //actually, sprite y is + 1
                disp.draw_sprite_scanline(if mem.ppu_reg.mask.sm() { 0 } else { 8 }, j);
            }
        }

        /*---------Post-render scanline-------*/
        master_cycles += dis_std.master_cycles_scanline as u32;
        cpu_cycles_end = master_cycles / dis_std.master_cycles_per_cpu as u32;
        cpu::execute_instruction_until(&mut cpu_reg, &mut mem, cpu_cycles_end);
        //sync horizon

        /*------------Vblank scanline------------*/
        disp::vblank(&mut cpu_reg, &mut mem);
        // execute code after NMI
        for _ in 0..dis_std.vblank_length {
            master_cycles += dis_std.master_cycles_scanline as u32;
            cpu_cycles_end = master_cycles / dis_std.master_cycles_per_cpu as u32;
            cpu::execute_instruction_until(&mut cpu_reg, &mut mem, cpu_cycles_end);
            //sync horizon
        }

        /*------------Pre-render scanlines------------*/
        //clear ppu status, vblank
        mem.ppu_reg.status.0 = 0;

        cpu::execute_instruction_until(
            &mut cpu_reg,
            &mut mem,
            dis_std.cpu_cycle_per_frame as u32 - if is_odd_frame { 3 } else { 0 },
        );
        //sync horizon

        if mem.ppu_reg.mask.bg() {
            ppu::cpoy_y_from_t_to_v(&mut mem.ppu_reg);
        }

        disp.display_present();
        is_odd_frame = !is_odd_frame;

        if input.get_key(&mut mem.key) == usize::MAX {
            break 'running;
        }

        let sleep_msec = Duration::from_millis(1_000 / dis_std.frame_rate as u64)
            .saturating_sub(start.elapsed());
        // println!(
        //     "Time elapsed: {:?}, sleep {:?}",
        //     start.elapsed(),
        //     sleep_msec
        // );
        std::thread::sleep(sleep_msec);
    }

    Ok(())
}
