#![allow(dead_code)]

pub mod ppu {
    use std::cell::RefCell;
    use std::rc::Rc;
    /*
    地址 	         大小     描述
    $0000-$0FFF 	$1000 	图案表 0
    $1000-$1FFF 	$1000 	图案表 1
    $2000-$23FF 	$0400 	名称表 0
    $2400-$27FF 	$0400 	名称表 1
    $2800-$2BFF 	$0400 	名称表 2
    $2C00-$2FFF 	$0400 	名称表 3
    $3000-$3EFF 	$0F00 	$2000-$2EFF 镜像
    $3F00-$3F1F 	$0020 	调色板内存索引
    $3F20-$3FFF 	$00E0 	$3F00-$3F1F 镜像
     */
    pub struct MemMap {
        pub bank: [Rc<RefCell<Vec<u8>>>; 16],
        pub palette_indx_tbl: [u8; 0x20],
        pub pseudo_cache: u8,
        pub oam: [u8; 0x100],
    }

    impl<'a> MemMap {
        pub fn new() -> Self {
            MemMap {
                bank: Default::default(),
                palette_indx_tbl: [0; 0x20],
                pseudo_cache: 0,
                oam: [0; 0x100],
            }
        }

        pub fn read(&mut self, addr: u16) -> u8 {
            let addr_map = addr & 0x3fff;

            if addr_map < 0x3f00 {
                let i: usize = (addr_map >> 10).into();
                let j: usize = (addr_map & 0x3ff).into();
                let val = self.pseudo_cache;
                let bank = self.bank[i].clone();

                self.pseudo_cache = bank.borrow()[j];
                val
            } else {
                let palet_addr: usize = ((addr_map - 0x3f00) & 0x1f).into();
                // self.pseudo_cache = self.palette_indx_tbl[palet_addr];
                self.palette_indx_tbl[palet_addr]
            }
        }

        pub fn write(&mut self, addr: u16, data: u8) {
            let addr_map = addr & 0x3fff;

            if addr_map < 0x3f00 {
                let i: usize = (addr_map >> 10).into();
                let j: usize = (addr_map & 0x3ff).into();
                let bank = self.bank[i].clone();

                bank.borrow_mut()[j] = data;
            } else {
                let palet_addr: usize = (addr_map & 0x1f).into();
                if palet_addr & 0x3 != 0 {
                    self.palette_indx_tbl[palet_addr] = data;
                } else {
                    if palet_addr & 0x10 == 0 {
                        self.palette_indx_tbl[palet_addr] = data;
                    } else {
                        self.palette_indx_tbl[palet_addr] = 64;
                    }
                }
            }
        }

        pub fn mapping_name_table(&mut self, four_screen: bool, mirror_flag: bool) {
            let mut vram_buff0 = Vec::new();
            vram_buff0.resize(1024, 0);
            let mut vram_buff1 = Vec::new();
            vram_buff1.resize(1024, 0);
            let vram = vec![
                Rc::new(RefCell::new(vram_buff0)),
                Rc::new(RefCell::new(vram_buff1)),
            ];

            if four_screen {
                // 4 屏
                let mut exvram_buff0 = Vec::new();
                exvram_buff0.resize(1024, 0);
                let mut exvram_buff1 = Vec::new();
                exvram_buff1.resize(1024, 0);
                let exvram = vec![
                    Rc::new(RefCell::new(exvram_buff0)),
                    Rc::new(RefCell::new(exvram_buff1)),
                ];
                self.bank[8] = vram[0].clone();
                self.bank[9] = vram[1].clone();
                self.bank[10] = exvram[0].clone();
                self.bank[11] = exvram[1].clone();
            } else if mirror_flag {
                // 横版
                self.bank[8] = vram[0].clone();
                self.bank[9] = vram[1].clone();
                self.bank[10] = vram[0].clone();
                self.bank[11] = vram[1].clone();
            } else {
                // 纵版
                self.bank[8] = vram[0].clone();
                self.bank[9] = vram[0].clone();
                self.bank[10] = vram[1].clone();
                self.bank[11] = vram[1].clone();
            }

            for i in 12..16 {
                //镜像地址
                self.bank[i] = self.bank[i - 4].clone();
            }
        }
    }

    use bitfield;

    /*
    PPUCTRL: WriteOnly
    7  bit  0
    ---- ----
    VPHB SINN
    |||| ||||
    |||| ||++- Base nametable address
    |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
    |||| |+--- VRAM address increment per CPU read/write of PPUDATA
    |||| |     (0: add 1, going across; 1: add 32, going down)
    |||| +---- Sprite pattern table address for 8x8 sprites
    ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
    |||+------ Background pattern table address (0: $0000; 1: $1000)
    ||+------- Sprite size (0: 8x8 pixels; 1: 8x16 pixels – see PPU OAM#Byte 1)
    |+-------- PPU master/slave select
    |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
    +--------- Generate an NMI at the start of the
            vertical blanking interval (0: off; 1: on)
    */
    bitfield! {
        pub struct CtrlReg(u8);
        impl Debug;
        u8;
        pub nn, set_nn: 1, 0; //nametable select, current nametable
        pub i, set_i: 2; //increment mode
        pub s, set_s: 3; //sprite tile select
        pub b, set_b: 4; //background tile select
        pub h, set_h: 5; //sprite heigh
        pub p, set_p: 6; //PPU master/slave
        pub v, set_v: 7; //NMI enable
    }

    /*
    PPUMASK: WriteOnly
    7  bit  0
    ---- ----
    BGRs bMmG
    |||| ||||
    |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
    |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
    |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
    |||| +---- 1: Show background
    |||+------ 1: Show sprites
    ||+------- Emphasize red (green on PAL/Dendy)
    |+-------- Emphasize green (red on PAL/Dendy)
    +--------- Emphasize blue
     */
    bitfield! {
        pub struct MaskReg(u8);
        impl Debug;
        u8;
        _, set_gs: 0; //greyscale
        _, set_bm: 1; //background left column enable
        _, set_sm: 2; //sprite left column enable
        _, set_bg: 3; //background enable
        _, set_s: 4; //sprite enable
        _, set_r: 5; //Emphasize red
        _, set_g: 6; //Emphasize green
        _, set_b: 7; //Emphasize blue
    }

    /*
    PPUSTATUS: ReadOnly
    * Do not read this address to wait for exactly one vertical redraw! On NTSC it
      will sometimes give false negatives, and on Dendy on some reboots it will always
      give false negatives. Additionally, on the PlayChoice 10, the Z80 supervisor will
      reject the game if the game leaves NMIs disabled for too long. Instead, use NMI for timing.
    7  bit  0
    ---- ----
    VSO. ....
    |||| ||||
    |||+-++++- PPU open bus. Returns stale PPU bus contents.
    ||+------- Sprite overflow. The intent was for this flag to be set
    ||         whenever more than eight sprites appear on a scanline, but a
    ||         hardware bug causes the actual behavior to be more complicated
    ||         and generate false positives as well as false negatives; see
    ||         PPU sprite evaluation. This flag is set during sprite
    ||         evaluation and cleared at dot 1 (the second dot) of the
    ||         pre-render line.
    |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
    |          a nonzero background pixel; cleared at dot 1 of the pre-render
    |          line.  Used for raster timing.
    +--------- Vertical blank has started (0: not in vblank; 1: in vblank).
            Set at dot 1 of line 241 (the line *after* the post-render
            line); cleared after reading $2002 and at dot 1 of the
            pre-render line.
    */
    bitfield! {
        pub struct StatusReg(u8);
        impl Debug;
        u8;
        o, _: 5; //sprite overflow
        s, _: 6; //sprite 0 hit
        pub v, set_v: 7; //vblank
    }

    // PPU 寄存器地址空间在 CPU 的寻址范围内，实际是属于 CPU 操作
    /*
    Common Name     Address  Bits
        PPUCTRL 	$2000 	VPHB SINN
        PPUMASK 	$2001 	BGRs bMmG
        PPUSTATUS 	$2002 	VSO- ----
        OAMADDR 	$2003 	aaaa aaaa
        OAMDATA 	$2004 	dddd dddd
        PPUSCROLL 	$2005 	xxxx xxxx
        PPUADDR 	$2006 	aaaa aaaa
        PPUDATA 	$2007 	dddd dddd
        OAMDMA   	$4014 	aaaa aaaa
     */
    pub struct Register {
        pub ctrl: CtrlReg,
        pub mask: MaskReg,
        pub status: StatusReg,
        pub oam_addr: u8,    //OAM adress, write only
        pub scroll: [u8; 2], //PPU scrolling position register, write twice: X scroll, Y scroll
        pub ppu_addr: u16, //PPU read/write address  two writes :most significant byte, least significant byte)
        pub ppu_addr_tmp: u16,
        pub dma_haddr: u8, //OAM DMA high address

        dw_cnt: u8, // double write count
    }

    impl Register {
        pub fn new() -> Self {
            Register {
                ctrl: CtrlReg(0),
                mask: MaskReg(0),
                status: StatusReg(0),
                oam_addr: 0,
                scroll: [0; 2],
                ppu_addr: 0,
                ppu_addr_tmp: 0,
                dma_haddr: 0,
                dw_cnt: 0,
            }
        }

        pub fn read(&mut self, mem: &mut MemMap, addr: u16) -> u8 {
            match addr & 0x0007 {
                0x0 | 0x1 | 0x3 | 0x5 | 0x6 => 0, //write only
                0x2 => {
                    let val = self.status.0;
                    self.status.set_v(false);
                    val
                }
                0x4 => mem.oam[self.oam_addr as usize],
                0x7 => {
                    let val = mem.read(self.ppu_addr);
                    self.ppu_addr += if self.ctrl.i() { 32 } else { 1 };
                    val
                }
                _ => unreachable!("Out of memory range!"),
            }
        }

        pub fn write(&mut self, mem: &mut MemMap, addr: u16, data: u8) {
            match addr & 0x0007 {
                0x0 => {
                    self.ctrl.0 = data;
                    self.ppu_addr_tmp =
                        (self.ppu_addr_tmp & 0xf3ff) | (((data as u16) & 0x03) << 10);
                }
                0x1 => {
                    self.status.0 = data;
                }
                0x2 => {}
                0x3 => {
                    self.oam_addr = data;
                }
                0x4 => {
                    mem.oam[self.oam_addr as usize] = data;
                    self.oam_addr += 1;
                }
                0x5 => {
                    if self.dw_cnt == 0 {
                        self.scroll[0] = data;
                        self.dw_cnt = 1;
                    } else {
                        self.scroll[1] = data;
                        self.dw_cnt = 0;
                    }
                }
                0x6 => {
                    //first write high byte, then write low byte.
                    if self.dw_cnt == 0 {
                        self.ppu_addr = (data as u16) << 8;
                        self.dw_cnt = 1;
                    } else {
                        self.ppu_addr |= data as u16;
                        self.dw_cnt = 0;
                    }
                }
                0x7 => {
                    mem.write(self.ppu_addr, data);
                    self.ppu_addr += if self.ctrl.i() { 32 } else { 1 };
                }
                _ => unreachable!("Out of memory range!"),
            }
        }
    }

    bitfield! {
        pub struct SpriteAttribute(u8);
        impl Debug;
        u8;
        pub h2bit, _: 1,0; // 调色板高两位 (类似于属性表)
        pub p, _: 5; // 优先级 (0-在背景前 1-在背景后)
        pub h, _: 6; // 1-水平翻转
        pub v, _: 7; // 1-垂直翻转
    }
    pub struct Sprite {
        pos_y: u8,
        tile_indx: u8,
        attr: SpriteAttribute,
        pos_x: u8,
    }

    impl Sprite {
        pub fn new(raw: &[u8]) -> Self {
            Self {
                pos_y: raw[0],
                tile_indx: raw[1],
                attr: SpriteAttribute(raw[2]),
                pos_x: raw[3],
            }
        }
    }

    // 背景 8*8 个像素为 1 个块，256*240 像素被分为 32*30 个块
    pub fn render_tile(
        pos_x: u16,
        pos_y: u16,
        ppu_reg: &Register,
        ppu_mem: &mut MemMap,
        color_indx: &mut [Vec<u8>; 8],
    ) {
        let tile_id = ((pos_x >> 3) + (pos_y >> 3) * 32) | 0x2000; //8*8 个像素为 1 个块，256*240 像素被分为 32*30 个块
        ppu_mem.read(tile_id); // refresh cache
        let mut pattern = ppu_mem.read(tile_id) as u16; //read again
        pattern <<= 4; //pattern table 以 16 bytes 为一个单位，存储像素的颜色索引，前 8 个 byte 存储低 1 个 bit，后 8 个 byte 存储高 1bit
        let ind = if ppu_reg.ctrl.b() { 0x1000_u16 } else { 0 };
        let mut offset0 = pattern + ind;
        let mut offset1 = pattern + 8 + ind;

        // 计算所在属性表
        // 1 个字节控制 4*4=16 个 tile，2bit 控制 2*2=4 个 tile。1 个字节控制 32*32 像素
        let attribute_id = (pos_x >> 5) + (pos_y >> 5) * 8;
        let attr = ppu_mem.read(attribute_id + 960 + 0x2000);
        let shift = (((pos_x & 0x10) >> 3) + ((pos_y & 0x10) >> 2)) as u8; //((pos_x&0x1f)>>4 + (pos_y&0x1f)>>4*2)*2;
        let color_h2bit = ((attr >> shift) & 0x3) << 2;
        for i in 0..8 {
            ppu_mem.read(offset0);
            let name0 = ppu_mem.read(offset0);
            ppu_mem.read(offset1);
            let name1 = ppu_mem.read(offset1);
            for j in 0..8 {
                //颜色索引高 2bit
                color_indx[i][j] = color_h2bit;
                //颜色索引低 2bit
                color_indx[i][j] |= (name0 >> (7 - j)) & 0x1;
                color_indx[i][j] |= ((name1 >> (7 - j)) & 0x1) << 1;
            }
            offset0 += 1;
            offset1 += 1;
        }
    }

    pub fn render_sprite(
        sprite_id: u8,
        ppu_reg: &Register,
        ppu_mem: &mut MemMap,
        color_indx: &mut [Vec<u8>; 8],
    ) -> (u16, u16) {
        let sprite = Sprite::new(&ppu_mem.oam[sprite_id as usize * 4..sprite_id as usize * 4 + 4]);
        if sprite.pos_y >= 0xef {
            return (sprite.pos_x as u16, sprite.pos_y as u16);
        }
        let h2bit = sprite.attr.h2bit() << 2;
        let ind = if ppu_reg.ctrl.s() { 0x1000_u16 } else { 0 };
        let mut offset0 = sprite.tile_indx as u16 + ind;
        let mut offset1 = sprite.tile_indx as u16 + 8 + ind;

        let y_range = if sprite.attr.v() {
            (0..8).rev().map(|n| n).collect::<Vec<_>>()
        } else {
            (0..8).map(|n| n).collect::<Vec<_>>()
        };
        let x_range = if sprite.attr.h() {
            (0..8).map(|n| n).collect::<Vec<_>>()
        } else {
            (0..8).rev().map(|n| n).collect::<Vec<_>>()
        };
        for y in y_range {
            ppu_mem.read(offset0);
            let name0 = ppu_mem.read(offset0);
            ppu_mem.read(offset1);
            let name1 = ppu_mem.read(offset1);
            let mut n = 0;
            for j in &x_range {
                //颜色索引高 2bit
                color_indx[y][n] = h2bit;
                //颜色索引低 2bit
                color_indx[y][n] |= (name0 >> j) & 0x1;
                color_indx[y][n] |= ((name1 >> j) & 0x1) << 1;
                n += 1;
            }
            offset0 += 1;
            offset1 += 1;
        }

        (sprite.pos_x as u16, sprite.pos_y as u16)
    }
}
