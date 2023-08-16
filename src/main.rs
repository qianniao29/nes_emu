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
    cpu::cpu_cycles_reset();
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

    'running: loop {
        for _n in 0..4000 {
            cpu::execute_one_instruction(&mut cpu_reg, &mut mem);
        }
        disp::vblank(&mut cpu_reg, &mut mem);
        disp.scanline_color_indx = [0; 256];
        //genert bg platette data
        disp.generate_palette_data(&mem.ppu_mem.palette_indx_tbl[0..16]);
        let mut sprite0_check_buf = [0_u8;8];
        let (sprite0_x, sprite0_y) = ppu::check_sprint0(&mem.ppu_reg,
            &mut mem.ppu_mem,&mut sprite0_check_buf);
        for j in 0..240 {
            // if mem.ppu_reg.mask.bg() == false {continue;}
            ppu::render_scanline(
                &mem.ppu_reg,
                &mut mem.ppu_mem,
                &mut disp.scanline_color_indx,
            );
            // sprite0 hit
            if(j == sprite0_y){
                // if sprite0_check_buf[0]

            }
            disp.draw_scanline(j);
            //dot 256, 257
            if mem.ppu_reg.mask.bg() {
                ppu::wrapping_around(&mut mem.ppu_reg);
                ppu::cpoy_x_from_t_to_v(&mut mem.ppu_reg);
            }
        }

        //genert sprite platette data
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
        disp.display_present();

        if input.get_key(&mut mem.key) == usize::MAX {
            break 'running;
        }

        ::std::thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }

    Ok(())
}
