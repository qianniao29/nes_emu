#![allow(dead_code)]

pub mod ppu {
    use crate::common::{Bus, Irq, Remap};
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
        pub oam2: [Sprite; 8],
    }

    impl Bus for MemMap {
        fn read(&mut self, addr: u16) -> u8 {
            if addr < 0x3f00 {
                let i: usize = (addr >> 10).into();
                let j: usize = (addr & 0x3ff).into();
                let val = self.pseudo_cache;
                let bank = self.bank[i].clone();

                self.pseudo_cache = bank.borrow()[j];
                val
            } else if addr <= 0x3fff {
                let palet_addr: usize = (addr & 0x1f).into();
                self.pseudo_cache = self.palette_indx_tbl[palet_addr];
                self.palette_indx_tbl[palet_addr]
            } else {
                panic!("Out of memory range!");
            }
        }

        fn write(&mut self, addr: u16, data: u8) {
            if addr < 0x3f00 {
                let i: usize = (addr >> 10).into();
                let j: usize = (addr & 0x3ff).into();
                let bank = self.bank[i].clone();

                bank.borrow_mut()[j] = data;
            } else if addr <= 0x3fff {
                let palet_addr: usize = (addr & 0x1f).into();
                if palet_addr & 0x3 != 0 {
                    self.palette_indx_tbl[palet_addr] = data;
                } else {
                    // 全局背景色，Addresses $3F10/$3F14/$3F18/$3F1C are mirrors of $3F00/$3F04/$3F08/$3F0C.
                    self.palette_indx_tbl[palet_addr] = data;
                    self.palette_indx_tbl[palet_addr ^ 0x10] = data;
                }
            } else {
                panic!("Out of memory range!");
            }
        }
    }

    impl Remap<'_, [Rc<RefCell<Vec<u8>>>; 16]> for MemMap {
        fn get_remap_mem(&mut self) -> &mut [Rc<RefCell<Vec<u8>>>; 16] {
            &mut self.bank
        }
    }

    impl<'a> MemMap {
        pub fn new() -> Self {
            MemMap {
                bank: Default::default(),
                palette_indx_tbl: [0; 0x20],
                pseudo_cache: 0,
                oam: [0; 0x100],
                oam2: Default::default(),
            }
        }

        pub fn read_direct(&self, addr: u16) -> u8 {
            let addr_map = addr & 0x3fff;

            if addr_map < 0x3f00 {
                let i: usize = (addr_map >> 10).into();
                let j: usize = (addr_map & 0x3ff).into();
                let bank = self.bank[i].clone();
                let val = bank.borrow()[j];
                val
            } else {
                let palet_addr: usize = (addr_map & 0x1f).into();
                self.palette_indx_tbl[palet_addr]
            }
        }
    }

    use bitfield::{self, Bit, BitMut};

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
        pub gs, set_gs: 0; //greyscale
        pub bm, set_bm: 1; //background left column enable
        pub sm, set_sm: 2; //sprite left column enable
        pub bg, set_bg: 3; //background enable
        pub s, set_s: 4; //sprite enable
        pub r, set_r: 5; //Emphasize red
        pub g, set_g: 6; //Emphasize green
        pub b, set_b: 7; //Emphasize blue
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
        pub o, set_o: 5; //sprite overflow
        pub s, set_s: 6; //sprite 0 hit
        pub v, set_v: 7; //vblank
    }

    /* ppu_addr
        yyy NN YYYYY XXXXX
        ||| || ||||| +++++-- coarse X scroll
        ||| || +++++-------- coarse Y scroll
        ||| ++-------------- nametable select
        +++----------------- fine Y scroll
    */
    bitfield! {
        pub struct PpuAddr(u16);
        impl Debug;
        u8;
        pub coarse_x, set_coarse_x: 4,0;
        pub coarse_y, set_coarse_y: 9,5;
        pub name_tbl_x, set_name_tbl_x: 10;
        pub name_tbl_y, set_name_tbl_y: 11;
        pub fine_y, set_fine_y: 14,12;

    }

    // PPU 寄存器地址空间在 CPU 的寻址范围内，实际是属于 CPU 操作
    /*
    Common Name Address Bits 	    Notes
    PPUCTRL 	$2000 	VPHB SINN 	NMI enable (V), PPU master/slave (P), sprite height (H),
                                    background tile select (B), sprite tile select (S),
                                    increment mode (I), nametable select (NN)
    PPUMASK 	$2001 	BGRs bMmG 	color emphasis (BGR), sprite enable (s), background enable (b),
                                    sprite left column enable (M), background left column enable (m), greyscale (G)
    PPUSTATUS 	$2002 	VSO- ---- 	vblank (V), sprite 0 hit (S), sprite overflow (O);
                                    read resets write pair for $2005/$2006
    OAMADDR 	$2003 	aaaa aaaa 	OAM read/write address
    OAMDATA 	$2004 	dddd dddd 	OAM data read/write
    PPUSCROLL 	$2005 	xxxx xxxx 	fine scroll position (two writes: X scroll, Y scroll)
    PPUADDR 	$2006 	aaaa aaaa 	PPU read/write address (two writes: most significant byte, least significant byte)
    PPUDATA 	$2007 	dddd dddd 	PPU data read/write
    OAMDMA  	$4014 	aaaa aaaa 	OAM DMA high address
     */
    pub struct Register {
        pub ctrl: CtrlReg,
        pub mask: MaskReg,
        pub status: StatusReg,
        pub oam_addr: u8,      //OAM adress, write only
        pub ppu_addr: PpuAddr, //PPU read/write address  two writes :most significant byte, least significant byte)
        pub ppu_addr_tmp: PpuAddr,
        pub fine_x_scroll: u8,
        second_write: bool, // double write count
    }

    impl Register {
        pub fn new() -> Self {
            Register {
                ctrl: CtrlReg(0),
                mask: MaskReg(0),
                status: StatusReg(0),
                oam_addr: 0,
                ppu_addr: PpuAddr(0),
                ppu_addr_tmp: PpuAddr(0),
                fine_x_scroll: 0,
                second_write: false,
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
                pos_y: raw[0], //actually display, sprite y is sprite[0] + 1
                tile_indx: raw[1],
                attr: SpriteAttribute(raw[2]),
                pos_x: raw[3],
            }
        }
        pub fn convert_from_slice(&mut self, raw: &[u8]) {
            self.pos_y = raw[0]; //actually display, sprite y is sprite[0] + 1
            self.tile_indx = raw[1];
            self.attr = SpriteAttribute(raw[2]);
            self.pos_x = raw[3];
        }
    }

    impl Default for Sprite {
        fn default() -> Self {
            Sprite {
                pos_y: 0xff,
                tile_indx: 0xff,
                attr: SpriteAttribute(0xff),
                pos_x: 0xff,
            }
        }
    }

    pub struct Ppu {
        pub reg: self::Register,
        pub mem: self::MemMap,
        pub irq: *mut dyn Irq,
    }

    impl Bus for Ppu {
        fn read(&mut self, addr: u16) -> u8 {
            match addr & 0x0007 {
                0x0 | 0x1 | 0x3 | 0x5 | 0x6 => 0, //write only
                0x2 => {
                    let val = self.reg.status.0;
                    self.reg.status.set_v(false);
                    self.reg.second_write = false;
                    val
                }
                0x4 => self.mem.oam[self.reg.oam_addr as usize],
                0x7 => {
                    let val = self.mem.read(self.reg.ppu_addr.0);
                    self.reg.ppu_addr.0 =
                        self.reg
                            .ppu_addr
                            .0
                            .wrapping_add(if self.reg.ctrl.i() { 32 } else { 1 });
                    val
                }
                _ => unreachable!("Out of memory range!"),
            }
        }

        fn write(&mut self, addr: u16, data: u8) {
            match addr & 0x0007 {
                0x0 => {
                    self.reg.ctrl.0 = data;
                    self.reg.ppu_addr_tmp.set_name_tbl_x(data & 0x1 == 0x01);
                    self.reg.ppu_addr_tmp.set_name_tbl_y(data & 0x2 == 0x02);
                }
                0x1 => {
                    self.reg.mask.0 = data;
                }
                0x2 => {}
                0x3 => {
                    self.reg.oam_addr = data;
                }
                0x4 => {
                    self.mem.oam[self.reg.oam_addr as usize] = data;
                    self.reg.oam_addr = self.reg.oam_addr.wrapping_add(1);
                }
                0x5 => {
                    // according to https://www.nesdev.org/wiki/PPU_scrolling
                    if self.reg.second_write == false {
                        // t: ........ ...HGFED = d: HGFED...
                        // x:               CBA = d: .....CBA
                        // w:                   = 1
                        self.reg.ppu_addr_tmp.set_coarse_x(data >> 3);
                        self.reg.fine_x_scroll = data & 0x7;
                        self.reg.second_write = true;
                    } else {
                        // t: .CBA..HG FED..... = d: HGFEDCBA
                        // w:                   = 0
                        self.reg.ppu_addr_tmp.set_coarse_y(data >> 3);
                        self.reg.ppu_addr_tmp.set_fine_y(data & 0x7);
                        self.reg.second_write = false;
                    }
                }
                0x6 => {
                    //first write high byte, then write low byte.
                    if self.reg.second_write == false {
                        // t: .CDEFGH ........ <- d: ..CDEFGH
                        //        <unused>     <- d: AB......
                        // t: Z...... ........ <- 0 (bit Z is cleared)
                        // w:                  <- 1
                        self.reg.ppu_addr_tmp.0 =
                            (self.reg.ppu_addr_tmp.0 & 0x80ff) | ((data as u16 & 0x3f) << 8);
                        self.reg.second_write = true;
                    } else {
                        // t: ....... ABCDEFGH <- d: ABCDEFGH
                        // v: <...all bits...> <- t: <...all bits...>
                        // w:                  <- 0
                        self.reg.ppu_addr_tmp.0 = (self.reg.ppu_addr_tmp.0 & 0xff00) | data as u16;
                        self.reg.ppu_addr.0 = self.reg.ppu_addr_tmp.0;
                        self.reg.second_write = false;
                    }
                }
                0x7 => {
                    self.mem.write(self.reg.ppu_addr.0, data);
                    self.reg.ppu_addr.0 =
                        self.reg
                            .ppu_addr
                            .0
                            .wrapping_add(if self.reg.ctrl.i() { 32 } else { 1 });
                }
                _ => unreachable!("Out of memory range!"),
            }
        }

        fn dma_write(&mut self, _: u16, src: &[u8], _: usize) {
            if self.reg.oam_addr == 0 {
                self.mem.oam.clone_from_slice(src);
            } else {
                let off = (255 - self.reg.oam_addr + 1) as usize;
                self.mem.oam[self.reg.oam_addr as usize..=255].clone_from_slice(&src[..off]);
                self.mem.oam[..self.reg.oam_addr as usize].clone_from_slice(&src[off..]);
            }
        }
    }

    impl Ppu {
        pub fn new(irq: *mut dyn Irq) -> Ppu {
            Ppu {
                reg: self::Register::new(),
                mem: self::MemMap::new(),
                irq,
            }
        }
        pub fn vblank(&mut self) {
            self.reg.status.set_v(true);
            if self.reg.ctrl.v() {
                unsafe {
                    (*self.irq).nmi_handler();
                }
            }
        }

        pub fn coarse_y_wrapping(&mut self) {
            // http://wiki.nesdev.com/w/index.php/PPU_scrolling#Wrapping_around
            if self.reg.ppu_addr.fine_y() >= 7 {
                self.reg.ppu_addr.set_fine_y(0);
                if self.reg.ppu_addr.coarse_y() == 29 {
                    self.reg.ppu_addr.set_coarse_y(0);
                    self.reg
                        .ppu_addr
                        .set_name_tbl_y(!self.reg.ppu_addr.name_tbl_y());
                } else if self.reg.ppu_addr.coarse_y() == 31 {
                    self.reg.ppu_addr.set_coarse_y(0);
                } else {
                    self.reg
                        .ppu_addr
                        .set_coarse_y(self.reg.ppu_addr.coarse_y() + 1);
                }
            } else {
                self.reg.ppu_addr.set_fine_y(self.reg.ppu_addr.fine_y() + 1);
            }
        }

        pub fn cpoy_x_from_t_to_v(&mut self) {
            self.reg
                .ppu_addr
                .set_coarse_x(self.reg.ppu_addr_tmp.coarse_x());
            self.reg
                .ppu_addr
                .set_name_tbl_x(self.reg.ppu_addr_tmp.name_tbl_x());
        }

        pub fn cpoy_y_from_t_to_v(&mut self) {
            self.reg
                .ppu_addr
                .set_coarse_y(self.reg.ppu_addr_tmp.coarse_y());
            self.reg.ppu_addr.set_fine_y(self.reg.ppu_addr_tmp.fine_y());
            self.reg
                .ppu_addr
                .set_name_tbl_y(self.reg.ppu_addr_tmp.name_tbl_y());
        }

        pub fn get_sprint0(&mut self, sprite0_buf: &mut [u8]) -> (u16, u16) {
            let sprite = Sprite::new(&self.mem.oam[0..4]);
            if sprite.pos_y >= 239 || sprite.pos_x == 255 {
                return (sprite.pos_x as u16, sprite.pos_y as u16);
            }
            let height;
            let mut tile = sprite.tile_indx as u16;
            let mut base = 0;
            let y_range;
            if self.reg.ctrl.h() {
                height = 16;
                y_range = if sprite.attr.v() {
                    &[15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
                } else {
                    &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
                };
                if tile & 0x1 != 0x00 {
                    base = 0x1000_u16;
                }
                //Tile number of top of sprite (0 to 254; bottom half gets the next tile)
                tile &= 0xfe;
            } else {
                height = 8;
                y_range = if sprite.attr.v() {
                    &[7, 6, 5, 4, 3, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0]
                } else {
                    &[0, 1, 2, 3, 4, 5, 6, 7, 0, 0, 0, 0, 0, 0, 0, 0]
                };
                if self.reg.ctrl.s() {
                    base = 0x1000_u16;
                }
            }
            let mut offset0 = (tile << 4) + base;

            let x_count = if sprite.pos_x > 248 {
                255 - sprite.pos_x + 1
            } else {
                8
            };
            let x_range = if sprite.attr.h() {
                &[0, 1, 2, 3, 4, 5, 6, 7]
            } else {
                &[7, 6, 5, 4, 3, 2, 1, 0]
            };

            for y in 0..height {
                //if 8x16 sprites, bottom half gets the next tile
                let pattern0 = self.mem.read_direct(offset0 + (y & 0x8));
                let pattern1 = self.mem.read_direct(offset0 + (y & 0x8) + 8);
                for j in 0..x_count as usize {
                    //颜色索引低 2bit 是否为 0，不为 0 的话就是不透明色
                    // sprite0_buf[y_range[y as usize]] |= (((pattern0 | pattern1) >> x_range[j]) & 0x1) << j;
                    sprite0_buf[y_range[y as usize]]
                        .set_bit(j, (pattern0 | pattern1).bit(x_range[j]));
                }
                offset0 += 1;
            }

            (sprite.pos_x as u16, sprite.pos_y as u16)
        }

        pub fn check_sprint0_hit(
            &mut self,
            y: u16,
            sprite0_x: u16,
            sprite0_y: u16,
            sprite0_buf: &[u8],
            bg_color_indx: &[u8],
        ) -> bool {
            if self.reg.mask.s() {
                if sprite0_x == 255 {
                    return false;
                }
                let height = if self.reg.ctrl.h() { 16 } else { 8 };
                if (self.reg.status.s() == false) && (y > sprite0_y) && (y <= sprite0_y + height) {
                    let check_bg = Self::check_backgroud(sprite0_x as usize, bg_color_indx);
                    let result = sprite0_buf[(y - sprite0_y - 1) as usize] & check_bg;

                    if (self.reg.mask.bm() == false || self.reg.mask.sm() == false)
                        && (sprite0_x <= 7)
                    {
                        // just judgement inrange 0~7
                        if result as u16 >> (8 - sprite0_x) == 0 {
                            return false;
                        }
                    }
                    if sprite0_x >= 248 {
                        if result & !(1 << (255 - sprite0_x)) != 0 {
                            return true;
                        } else {
                            return false;
                        }
                    } else if result != 0 {
                        return true;
                    }
                }
            }
            return false;
        }

        pub fn check_backgroud(x: usize, color_indx: &[u8]) -> u8 {
            let mut check_bg = 0;
            let x_range_max = if x > 248 { 255 - x + 1 } else { 8 };

            for i in 0..x_range_max {
                // check_bg |= ((color_indx[x + i] | (color_indx[x + i] >> 1)) & 0x1) << i;
                check_bg.set_bit(i, (color_indx[x + i] & 0x3) != 0);
            }
            check_bg
        }

        /*
        计算所在属性表
        1 个字节控制 4*4=16 个 tile，2bit 控制 2*2=4 个 tile。1 个字节控制 32*32 像素
        */
        #[inline]
        fn calc_color_h2bit(&self) -> u8 {
            let tile_x = self.reg.ppu_addr.coarse_x() as u16;
            let tile_y = self.reg.ppu_addr.coarse_y() as u16;
            let attribute_id = (tile_x >> 2) + (tile_y >> 2) * 8;
            let attr = self
                .mem
                .read_direct(attribute_id + 960 + 0x2000 + (self.reg.ppu_addr.0 & 0xc00));
            let shift = ((tile_x & 0x2) + ((tile_y & 0x2) << 1)) as u8; //((pos_x&0x1f)>>4 + (pos_y&0x1f)>>4*2)*2;
            ((attr >> shift) & 0x3) << 2
        }
        #[inline]
        fn calc_pattern(&self) -> u16 {
            let name_tbl_addr = 0x2000 | (self.reg.ppu_addr.0 & 0xfff); //8*8 个像素为 1 个块，256*240 像素被分为 32*30 个块
            let base = if self.reg.ctrl.b() { 0x1000_u16 } else { 0 };
            // read name table
            let mut pattern_addr = self.mem.read_direct(name_tbl_addr) as u16;
            pattern_addr <<= 4; //pattern table 以 16 bytes 为一个单位，存储像素的颜色索引，前 8 个 byte 存储低 1 个 bit，后 8 个 byte 存储高 1bit
            pattern_addr + base + self.reg.ppu_addr.fine_y() as u16
        }
        #[inline]
        fn coarse_x_wrapping(&mut self) {
            if self.reg.ppu_addr.coarse_x() >= 31 {
                self.reg.ppu_addr.set_coarse_x(0);
                self.reg
                    .ppu_addr
                    .set_name_tbl_x(!self.reg.ppu_addr.name_tbl_x());
            } else {
                self.reg
                    .ppu_addr
                    .set_coarse_x(self.reg.ppu_addr.coarse_x() + 1);
            }
        }

        pub fn render_bg_scanline(&mut self, color_indx: &mut [u8]) {
            let mut buf_ind = 0;
            let fine_x = self.reg.fine_x_scroll as usize;
            let tile_x_is_odd = self.reg.ppu_addr.coarse_x().bit(0);

            let mut fill_color =
                |tile_count_1_or_2: usize, pixel_start: &[usize], pixel_end: &[usize]| {
                    let color_h2bit = self.calc_color_h2bit();
                    for i in 0..tile_count_1_or_2 {
                        let offset = self.calc_pattern();
                        self.coarse_x_wrapping();

                        let pattern0 = self.mem.read_direct(offset);
                        let pattern1 = self.mem.read_direct(offset + 8);
                        for j in pixel_start[i]..pixel_end[i] {
                            //颜色索引高 2bit
                            color_indx[buf_ind] = color_h2bit;
                            //颜色索引低 2bit
                            color_indx[buf_ind] |= (pattern0 >> (7 - j)) & 0x1;
                            color_indx[buf_ind] |= ((pattern1 >> (7 - j)) & 0x1) << 1;
                            buf_ind += 1;
                        }
                    }
                };

            if tile_x_is_odd == false && fine_x == 0 {
                for _ in 0..16 {
                    fill_color(2, &[0, 0], &[8, 8]);
                }
            } else if tile_x_is_odd && fine_x != 0 {
                fill_color(1, &[fine_x], &[8]);
                for _ in 1..16 {
                    fill_color(2, &[0, 0], &[8, 8]);
                }
                fill_color(2, &[0, 0], &[8, fine_x]);
            } else if tile_x_is_odd && fine_x == 0 {
                fill_color(1, &[0], &[8]);
                for _ in 1..16 {
                    fill_color(2, &[0, 0], &[8, 8]);
                }
                fill_color(1, &[0], &[8]);
            } else if tile_x_is_odd == false && fine_x != 0 {
                fill_color(2, &[fine_x, 0], &[8, 8]);
                for _ in 1..16 {
                    fill_color(2, &[0, 0], &[8, 8]);
                }
                fill_color(1, &[0], &[fine_x]);
            }
        }

        pub fn render_sprite_scanline(&mut self, y: u16, color_indx: &mut [u8]) {
            if y > 0xef {
                return;
            }
            let line = y as u8;
            let mut count = 0;
            let mut m = 0;
            let height = if self.reg.ctrl.h() { 16 } else { 8 };
            let mut is_bg_displayable = [0_u8; 8];

            for n in 0..64 {
                let sprite = &self.mem.oam[(n * 4)..(n * 4 + 4)];

                if (sprite[m] <= line) && (line < (sprite[m] + height)) {
                    if count < 8 {
                        self.mem.oam2[count].convert_from_slice(sprite);
                        is_bg_displayable[count] =
                            Self::check_backgroud(sprite[3] as usize, &color_indx);
                    } else {
                        //set overflow
                        self.reg.status.set_o(true);
                    }
                    count += 1;
                } else if count >= 8 {
                    m = (m + 1) & 0x3;
                }
            }

            if self.reg.mask.s() == false {
                return;
            }
            color_indx.copy_from_slice(&[0; 256]);
            if count > 8 {
                count = 8
            }

            let mut base = if (self.reg.ctrl.h() == false) && (self.reg.ctrl.s() == true) {
                0x1000_u16
            } else {
                0
            };
            for i in (0..count).rev() {
                let sprite = &self.mem.oam2[i];
                let h2bit = sprite.attr.h2bit() << 2;
                let mut tile = sprite.tile_indx as u16;
                if self.reg.ctrl.h() {
                    /* Tile number of top of sprite (0 to 254; bottom half gets the next tile)
                        $00: $0000-$001F
                        $01: $1000-$101F
                        $02: $0020-$003F
                        $03: $1020-$103F
                        $04: $0040-$005F
                        [...]
                        $FE: $0FE0-$0FFF
                        $FF: $1FE0-$1FFF
                    */
                    if tile & 0x1 != 0x00 {
                        base = 0x1000_u16;
                    } else {
                        base = 0;
                    }
                    tile &= 0xfe;
                }
                let mut offset0 = (tile << 4) + base;
                let mut t = line - sprite.pos_y;

                if sprite.attr.v() {
                    t = height - 1 - t;
                }
                //bottom half gets the next tile
                offset0 += ((t & 0x7) + ((t & 0x8) << 2)) as u16;

                let x_ount = if sprite.pos_x > 248 {
                    255 - sprite.pos_x + 1
                } else {
                    8
                };
                let x_range = if sprite.attr.h() {
                    &[0, 1, 2, 3, 4, 5, 6, 7]
                } else {
                    &[7, 6, 5, 4, 3, 2, 1, 0]
                };

                let slice_color_indx = &mut color_indx
                    [(sprite.pos_x as usize)..(sprite.pos_x as usize + x_ount as usize)];
                let pattern0 = self.mem.read_direct(offset0);
                let pattern1 = self.mem.read_direct(offset0 + 8);
                for j in 0..x_ount as usize {
                    if sprite.attr.p() && is_bg_displayable[i].bit(j) {
                        continue;
                    }
                    //颜色索引高 2bit
                    slice_color_indx[j] = h2bit;
                    //颜色索引低 2bit
                    slice_color_indx[j] |=
                        ((pattern0 >> x_range[j]) & 0x1) | (((pattern1 >> x_range[j]) & 0x1) << 1);
                }
            }
        }
    }
}
