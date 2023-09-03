#![allow(dead_code)]

pub mod ppu {
    use ahash::AHashMap;
    use std::cell::RefCell;
    use std::default;
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
                self.pseudo_cache = self.palette_indx_tbl[palet_addr];
                self.palette_indx_tbl[palet_addr]
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
                let palet_addr: usize = ((addr_map - 0x3f00) & 0x1f).into();
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
                } else if palet_addr == 0 {
                    // 全局背景色
                    for n in [0, 4, 8, 12] {
                        self.palette_indx_tbl[n] = data;
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

        pub fn read(&mut self, mem: &mut MemMap, addr: u16) -> u8 {
            match addr & 0x0007 {
                0x0 | 0x1 | 0x3 | 0x5 | 0x6 => 0, //write only
                0x2 => {
                    let val = self.status.0;
                    self.status.set_v(false);
                    self.second_write = false;
                    val
                }
                0x4 => mem.oam[self.oam_addr as usize],
                0x7 => {
                    let val = mem.read(self.ppu_addr.0);
                    self.ppu_addr.0 =
                        self.ppu_addr
                            .0
                            .wrapping_add(if self.ctrl.i() { 32 } else { 1 });
                    val
                }
                _ => unreachable!("Out of memory range!"),
            }
        }

        pub fn write(&mut self, mem: &mut MemMap, addr: u16, data: u8) {
            match addr & 0x0007 {
                0x0 => {
                    self.ctrl.0 = data;
                    self.ppu_addr_tmp.set_name_tbl_x(data & 0x1 == 0x01);
                    self.ppu_addr_tmp.set_name_tbl_y(data & 0x2 == 0x02);
                }
                0x1 => {
                    self.mask.0 = data;
                }
                0x2 => {}
                0x3 => {
                    self.oam_addr = data;
                }
                0x4 => {
                    mem.oam[self.oam_addr as usize] = data;
                    self.oam_addr = self.oam_addr.wrapping_add(1);
                }
                0x5 => {
                    // according to https://www.nesdev.org/wiki/PPU_scrolling
                    if self.second_write == false {
                        // t: ........ ...HGFED = d: HGFED...
                        // x:               CBA = d: .....CBA
                        // w:                   = 1
                        self.ppu_addr_tmp.set_coarse_x(data >> 3);
                        self.fine_x_scroll = data & 0x7;
                        self.second_write = true;
                    } else {
                        // t: .CBA..HG FED..... = d: HGFEDCBA
                        // w:                   = 0
                        self.ppu_addr_tmp.set_coarse_y(data >> 3);
                        self.ppu_addr_tmp.set_fine_y(data & 0x7);
                        self.second_write = false;
                    }
                }
                0x6 => {
                    //first write high byte, then write low byte.
                    if self.second_write == false {
                        // t: .CDEFGH ........ <- d: ..CDEFGH
                        //        <unused>     <- d: AB......
                        // t: Z...... ........ <- 0 (bit Z is cleared)
                        // w:                  <- 1
                        self.ppu_addr_tmp.0 =
                            (self.ppu_addr_tmp.0 & 0x80ff) | ((data as u16 & 0x3f) << 8);
                        self.second_write = true;
                    } else {
                        // t: ....... ABCDEFGH <- d: ABCDEFGH
                        // v: <...all bits...> <- t: <...all bits...>
                        // w:                  <- 0
                        self.ppu_addr_tmp.0 = (self.ppu_addr_tmp.0 & 0xff00) | data as u16;
                        self.ppu_addr.0 = self.ppu_addr_tmp.0;
                        self.second_write = false;
                    }
                }
                0x7 => {
                    mem.write(self.ppu_addr.0, data);
                    self.ppu_addr.0 =
                        self.ppu_addr
                            .0
                            .wrapping_add(if self.ctrl.i() { 32 } else { 1 });
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
                pos_y: raw[0] + 1, //actually, sprite y is sprite[0] + 1
                tile_indx: raw[1],
                attr: SpriteAttribute(raw[2]),
                pos_x: raw[3],
            }
        }
        pub fn convert_from_slice(&mut self, raw: &[u8]) {
            self.pos_y = raw[0] + 1; //actually, sprite y is sprite[0] + 1
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
        let mut offset1 = offset0 + 8;

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

    pub fn coarse_y_wrapping(ppu_reg: &mut Register) {
        // http://wiki.nesdev.com/w/index.php/PPU_scrolling#Wrapping_around
        if ppu_reg.ppu_addr.fine_y() >= 7 {
            ppu_reg.ppu_addr.set_fine_y(0);
            if ppu_reg.ppu_addr.coarse_y() == 29 {
                ppu_reg.ppu_addr.set_coarse_y(0);
                ppu_reg
                    .ppu_addr
                    .set_name_tbl_y(!ppu_reg.ppu_addr.name_tbl_y());
            } else if ppu_reg.ppu_addr.coarse_y() == 31 {
                ppu_reg.ppu_addr.set_coarse_y(0);
            } else {
                ppu_reg
                    .ppu_addr
                    .set_coarse_y(ppu_reg.ppu_addr.coarse_y() + 1);
            }
        } else {
            ppu_reg.ppu_addr.set_fine_y(ppu_reg.ppu_addr.fine_y() + 1);
        }
    }

    pub fn cpoy_x_from_t_to_v(ppu_reg: &mut Register) {
        ppu_reg
            .ppu_addr
            .set_coarse_x(ppu_reg.ppu_addr_tmp.coarse_x());
        ppu_reg
            .ppu_addr
            .set_name_tbl_x(ppu_reg.ppu_addr_tmp.name_tbl_x());
    }

    pub fn cpoy_y_from_t_to_v(ppu_reg: &mut Register) {
        ppu_reg
            .ppu_addr
            .set_coarse_y(ppu_reg.ppu_addr_tmp.coarse_y());
        ppu_reg.ppu_addr.set_fine_y(ppu_reg.ppu_addr_tmp.fine_y());
        ppu_reg
            .ppu_addr
            .set_name_tbl_y(ppu_reg.ppu_addr_tmp.name_tbl_y());
    }

    pub fn get_sprint0(
        ppu_reg: &Register,
        ppu_mem: &mut MemMap,
        sprite0_buf: &mut [u8],
    ) -> (u16, u16) {
        let sprite = Sprite::new(&ppu_mem.oam[0..4]);
        if sprite.pos_y >= 239 {
            return (sprite.pos_x as u16, sprite.pos_y as u16);
        }
        let ind = if ppu_reg.ctrl.s() { 0x1000_u16 } else { 0 };
        let mut offset0 = ((sprite.tile_indx as u16) << 4) + ind;
        let mut offset1 = offset0 + 8;

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
            let pattern0 = ppu_mem.read_direct(offset0);
            let pattern1 = ppu_mem.read_direct(offset1);
            for j in &x_range {
                //颜色索引低 2bit 是否为 0，不为 0 的话就是不透明色
                sprite0_buf[y] |= (pattern0 | pattern1) & (0x1 << j);
            }
            offset0 += 1;
            offset1 += 1;
        }

        (sprite.pos_x as u16, sprite.pos_y as u16)
    }

    pub fn check_sprint0_hit(
        y: u16,
        sprite0_x: u16,
        sprite0_y: u16,
        ppu_reg: &mut Register,
        sprite0_buf: &[u8],
        bg_color_indx: &[u8],
    ) -> bool {
        if ppu_reg.mask.s() {
            if (ppu_reg.status.s() == false) && (y >= sprite0_y) && (y < sprite0_y + 8) {
                if ppu_reg.mask.bm() == false || ppu_reg.mask.sm() == false {
                    if sprite0_x <= 7 {
                        return false;
                    }
                }
                let check_bg = check_backgroud(sprite0_x as usize, bg_color_indx);
                if sprite0_buf[0] & check_bg != 0 {
                    return true;
                }
            }
        }
        return false;
    }

    pub fn check_backgroud(x: usize, color_indx: &[u8]) -> u8 {
        let mut check_bg = 0;

        for i in 0..8 {
            check_bg |= ((color_indx[x + i] | (color_indx[x + i] >> 1)) & 0x1) << i;
        }
        check_bg
    }

    pub fn check_sprite_overflow(ppu_reg: &Register, ppu_mem: &MemMap) -> u16 {
        let mut cnt_map: AHashMap<u8, u8> = AHashMap::new();
        let height = if ppu_reg.ctrl.h() { 16 } else { 8 };

        let mut pos_y: u16;
        for i in 0..64 {
            for j in 0..height {
                pos_y = ppu_mem.oam[4 * i] as u16 + j;
                if pos_y > 240 {
                    continue;
                }
                let cnt = cnt_map.entry(pos_y as u8).or_insert(1);
                *cnt = *cnt + 1;
                if *cnt > 8 {
                    return pos_y as u16;
                }
            }
        }
        return 0xffff;
    }

    /*
    计算所在属性表
    1 个字节控制 4*4=16 个 tile，2bit 控制 2*2=4 个 tile。1 个字节控制 32*32 像素
    */
    fn calc_color_h2bit(tile_x: u16, tile_y: u16, ppu_mem: &MemMap) -> u8 {
        let attribute_id = (tile_x >> 2) + (tile_y >> 2) * 8;
        let attr = ppu_mem.read_direct(attribute_id + 960 + 0x2000);
        let shift = ((tile_x & 0x2) + ((tile_y & 0x2) << 1)) as u8; //((pos_x&0x1f)>>4 + (pos_y&0x1f)>>4*2)*2;
        ((attr >> shift) & 0x3) << 2
    }
    fn calc_pattern(ppu_reg: &Register, ppu_mem: &MemMap) -> u16 {
        let name_tbl_addr = 0x2000 | (ppu_reg.ppu_addr.0 & 0xfff); //8*8 个像素为 1 个块，256*240 像素被分为 32*30 个块
        let base = if ppu_reg.ctrl.b() { 0x1000_u16 } else { 0 };
        // read name table
        let mut pattern_addr = ppu_mem.read_direct(name_tbl_addr) as u16;
        pattern_addr <<= 4; //pattern table 以 16 bytes 为一个单位，存储像素的颜色索引，前 8 个 byte 存储低 1 个 bit，后 8 个 byte 存储高 1bit
        pattern_addr + base + ppu_reg.ppu_addr.fine_y() as u16
    }
    fn coarse_x_wrapping(ppu_reg: &mut Register) {
        if ppu_reg.ppu_addr.coarse_x() >= 31 {
            ppu_reg.ppu_addr.set_coarse_x(0);
            ppu_reg
                .ppu_addr
                .set_name_tbl_x(!ppu_reg.ppu_addr.name_tbl_x());
        } else {
            ppu_reg
                .ppu_addr
                .set_coarse_x(ppu_reg.ppu_addr.coarse_x() + 1);
        }
    }

    pub fn render_bg_scanline(ppu_reg: &mut Register, ppu_mem: &MemMap, color_indx: &mut [u8]) {
        // if ppu_reg.mask.bg() == false {
        //     //TODO: 放在函数外的循环前面？
        //     return;
        // }

        let mut buf_ind = 0;
        let fine_x = ppu_reg.fine_x_scroll as usize;
        let tile_y = ppu_reg.ppu_addr.coarse_y() as u16;

        let mut fill_color = |tile_nums: usize, pixel_start: usize, pixel_end: usize| {
            if pixel_end == pixel_start {
                return;
            }
            let tile_x = ppu_reg.ppu_addr.coarse_x() as u16;
            let color_h2bit = calc_color_h2bit(tile_x, tile_y, ppu_mem);

            for _ in 0..tile_nums {
                let offset = calc_pattern(ppu_reg, ppu_mem);
                coarse_x_wrapping(ppu_reg);

                let pattern0 = ppu_mem.read_direct(offset);
                let pattern1 = ppu_mem.read_direct(offset + 8);
                for j in pixel_start..pixel_end {
                    //颜色索引高 2bit
                    color_indx[buf_ind] = color_h2bit;
                    //颜色索引低 2bit
                    color_indx[buf_ind] |= (pattern0 >> (7 - j)) & 0x1;
                    color_indx[buf_ind] |= ((pattern1 >> (7 - j)) & 0x1) << 1;
                    buf_ind += 1;
                }
            }
        };

        if fine_x == 0 {
            for _ in 0..16 {
                fill_color(2, 0, 8);
            }
        } else {
            fill_color(1, fine_x, 8);
            fill_color(1, 0, 8);
            for _ in 1..16 {
                fill_color(2, 0, 8);
            }
            fill_color(1, 0, fine_x);
        }
    }

    pub fn render_sprite(
        sprite_id: u8,
        ppu_reg: &Register,
        ppu_mem: &mut MemMap,
        color_indx: &mut [u8],
    ) -> (u16, u16, bool) {
        let sprite = Sprite::new(&ppu_mem.oam[sprite_id as usize * 4..sprite_id as usize * 4 + 4]);
        if sprite.pos_y > 0xef || sprite.attr.p() {
            return (sprite.pos_x as u16, sprite.pos_y as u16, false);
        }

        let h2bit = sprite.attr.h2bit() << 2;
        let ind = if ppu_reg.ctrl.s() { 0x1000_u16 } else { 0 };
        let mut offset0 = ((sprite.tile_indx as u16) << 4) + ind;
        let mut offset1 = offset0 + 8;

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
            let line_color_indx = &mut color_indx[(y * 8)..(y * 8 + 8)];
            ppu_mem.read(offset0);
            let name0 = ppu_mem.read(offset0);
            ppu_mem.read(offset1);
            let name1 = ppu_mem.read(offset1);
            let mut n = 0;
            for j in &x_range {
                //颜色索引高 2bit
                line_color_indx[n] = h2bit;
                //颜色索引低 2bit
                line_color_indx[n] |= ((name0 >> j) & 0x1) | (((name1 >> j) & 0x1) << 1);
                n += 1;
            }
            offset0 += 1;
            offset1 += 1;
        }

        (sprite.pos_x as u16, sprite.pos_y as u16, true)
    }

    pub fn render_sprite_scanline(
        y: u16,
        ppu_reg: &mut Register,
        ppu_mem: &mut MemMap,
        color_indx: &mut [u8],
    ) {
        if y >= 0xef {
            return;
        }
        if (ppu_reg.mask.bg() == false) && (ppu_reg.mask.s() == false) {
            return;
        }
        let line = y as u8;
        let mut count = 0;
        let mut m = 0;
        let height = if ppu_reg.ctrl.h() { 16 } else { 8 };

        ppu_mem.oam2 = Default::default();
        for n in 0..64 {
            let sprite = &ppu_mem.oam[n * 4..n * 4 + 4];
            //actually, sprite y is sprite[m] + 1
            if (sprite[m] < line) && (line < (sprite[m] + height + 1)) {
                if count < 8 {
                    ppu_mem.oam2[count].convert_from_slice(sprite);
                } else {
                    //set overflow
                    ppu_reg.status.set_o(true);
                    m = (m + 4) & 0x3;
                }
                count += 1;
            } else if count >= 8 {
                m = (m + 1) & 0x3;
            }
        }
        if ppu_reg.mask.s() == false {
            return;
        }
        if count > 8 {
            count = 8
        }
        for i in (0..count).rev() {
            let sprite = &ppu_mem.oam2[i];
            if sprite.attr.p() {
                continue;
            }

            let h2bit = sprite.attr.h2bit() << 2;
            let mut tile = sprite.tile_indx as u16;
            let mut base = 0;
            if ppu_reg.ctrl.h() {
                if tile & 0x1 == 0x00 {
                    base = 0x1000_u16;
                }
                tile &= 0xfe;
            } else {
                if ppu_reg.ctrl.s() {
                    base = 0x1000_u16;
                }
            }
            let mut offset0 = (tile * (height as u16 * 2)) + base;

            if sprite.attr.v() {
                offset0 += height as u16 - (line - sprite.pos_y) as u16;
            } else {
                offset0 += (line - sprite.pos_y) as u16;
            };

            let x_range = if sprite.attr.h() {
                (0..8).map(|n| n).collect::<Vec<_>>()
            } else {
                (0..8).rev().map(|n| n).collect::<Vec<_>>()
            };

            let line_color_indx = &mut color_indx[sprite.pos_x as usize..(sprite.pos_x as usize + 8)];
            let pattern0 = ppu_mem.read_direct(offset0);
            let pattern1 = ppu_mem.read_direct(offset0 + height as u16);
            let mut n = 0;
            for j in &x_range {
                //颜色索引高 2bit
                line_color_indx[n] = h2bit;
                //颜色索引低 2bit
                line_color_indx[n] |= ((pattern0 >> j) & 0x1) | (((pattern1 >> j) & 0x1) << 1);
                n += 1;
            }
        }
    }
}
