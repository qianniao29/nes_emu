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

use std::cell::RefCell;
use std::env;
use std::rc::Rc;
use std::time::Duration;

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

    /*-----------------------display init-----------------------------*/
    let mut disp = disp_sdl2::DispSDL2::new();
    /*--------------------------------------------------------*/
    let mut input = input_sdl2::InputSDL2::new(&disp.dev.sdl_context);

    let dis_std = disp::NTSC;

    'running: loop {
        let mut master_cycles: u32 = 0;
        cpu::cpu_cycles_reset();

        disp.scanline_color_indx = [0; 256];

        //genert bg platette data
        disp.generate_palette_data(&mem.ppu_mem.palette_indx_tbl[0..16]);
        let mut sprite0_check_buf = [0_u8; 8];
        let (sprite0_x, sprite0_y) =
            ppu::check_sprint0(&mem.ppu_reg, &mut mem.ppu_mem, &mut sprite0_check_buf);
        let sprite_ov_line = ppu::check_sprite_overflow(&mem.ppu_reg, &mem.ppu_mem);
        // if sprite_ov_line != 0xffff {
        //     println!("sprite_ov_line={}", sprite_ov_line);
        // }
        for j in 0..240 {
            // if mem.ppu_reg.mask.bg() {
            ppu::render_scanline(
                &mem.ppu_reg,
                &mut mem.ppu_mem,
                &mut disp.scanline_color_indx,
            );
            disp.draw_scanline(j);
            // sprite0 hit
            if (j >= sprite0_y) && (j < sprite0_y + 8) {
                let check_bg = ppu::check_backgroud(sprite0_x as usize, &disp.scanline_color_indx);
                if (mem.ppu_reg.status.s() == false) && (sprite0_check_buf[0] & check_bg != 0) {
                    mem.ppu_reg.status.set_s(true);
                }
            }
            // }
            if sprite_ov_line == j {
                mem.ppu_reg.status.set_o(true);
            }
            // execute code in one scanline cycles
            master_cycles += dis_std.master_cycles_scanline as u32;
            let cpu_cycles_end = master_cycles / dis_std.master_cycles_per_cpu as u32;
            while cpu::get_cpu_cycles() < cpu_cycles_end {
                cpu::execute_one_instruction(&mut cpu_reg, &mut mem);
            }
            //dot 256, 257
            // if mem.ppu_reg.mask.bg() {
            ppu::coarse_y_wrapping(&mut mem.ppu_reg);
            ppu::cpoy_x_from_t_to_v(&mut mem.ppu_reg);
            // }
            //sync horizon
        }

        // execute code in one scanline cycles
        master_cycles += dis_std.master_cycles_scanline as u32;
        let cpu_cycles_end = master_cycles / dis_std.master_cycles_per_cpu as u32;
        while cpu::get_cpu_cycles() < cpu_cycles_end {
            cpu::execute_one_instruction(&mut cpu_reg, &mut mem);
        }
        //sync horizon

        //start vblank
        disp::vblank(&mut cpu_reg, &mut mem);
        // execute code after NMI
        for _ in 0..dis_std.vblank_length {
            master_cycles += dis_std.master_cycles_scanline as u32;
            let cpu_cycles_end = master_cycles / dis_std.master_cycles_per_cpu as u32;
            while cpu::get_cpu_cycles() < cpu_cycles_end {
                cpu::execute_one_instruction(&mut cpu_reg, &mut mem);
            }
            //sync horizon
        }
        //clear ppu status
        mem.ppu_reg.status.0 = 0;

        //Pre-render
        master_cycles += dis_std.cpu_cycle_per_frame as u32;
        let cpu_cycles_end = master_cycles / dis_std.master_cycles_per_cpu as u32;
        while cpu::get_cpu_cycles() < cpu_cycles_end {
            cpu::execute_one_instruction(&mut cpu_reg, &mut mem);
        }
        //sync horizon

        //scanline 261 clear vblank
        // if mem.ppu_reg.mask.bg() {
        ppu::cpoy_y_from_t_to_v(&mut mem.ppu_reg);
        // }

        // if mem.ppu_reg.mask.s() {
        // genert sprite platette data
        disp.generate_palette_data(&mem.ppu_mem.palette_indx_tbl[16..32]);
        for id in (0..64).rev() {
            let (x, y) = ppu::render_sprite(
                id,
                &mem.ppu_reg,
                &mut mem.ppu_mem,
                &mut disp.scanline_color_indx,
            );
            if y >= 0xef {
                continue;
            }
            disp.draw_sprite(x, y);
        }
        // }
        disp.display_present();

        if input.get_key(&mut mem.key) == usize::MAX {
            break 'running;
        }

        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }

    Ok(())
}
