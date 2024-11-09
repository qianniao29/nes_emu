pub mod mapper_mmc1 {

    use crate::{
        common::Bus,
        mapper::mapper::{MapperBase, MapperFunc, ScreenMode},
    };
    use std::cell::RefCell;
    use std::rc::Rc;

    /*
        4bit0
        -----
        CPPMM
        |||||
        |||++- Nametable arrangement: (0: one-screen, lower bank; 1: one-screen, upper bank;
        |||               2: horizontal arrangement ("vertical mirroring", PPU A10);
        |||               3: vertical arrangement ("horizontal mirroring", PPU A11) )
        |++--- PRG ROM bank mode (0, 1: switch 32 KB at $8000, ignoring low bit of bank number;
        |                         2: fix first bank at $8000 and switch 16 KB bank at $C000;
        |                         3: fix last bank at $C000 and switch 16 KB bank at $8000)
        +----- CHR ROM bank mode (0: switch 8 KB at a time; 1: switch two separate 4 KB banks)
    */
    bitfield! {
        pub struct CtrlReg(u8);
        impl Debug;
        u8;
        pub name_tbl_arrang, _: 1, 0;    //Nametable arrangement
        pub prg_mode, set_prg_mode: 3,2; //PRG ROM bank mode
        pub chr_mode, _: 4;              //CHR ROM bank mode
    }

    /**************************************************************************************
     * CPU $6000-$7FFF: 8 KB PRG RAM bank, (optional)
     * CPU $8000-$BFFF: 16 KB PRG ROM bank, either switchable or fixed to the first bank
     * CPU $C000-$FFFF: 16 KB PRG ROM bank, either fixed to the last bank or switchable
     * PPU $0000-$0FFF: 4 KB switchable CHR bank
     * PPU $1000-$1FFF: 4 KB switchable CHR bank
     **************************************************************************************/

    pub struct MapperMmc1 {
        pub base: MapperBase,
        control: CtrlReg,
        shifter: u8,
    }

    impl MapperMmc1 {
        fn remapping_name_table(&mut self) {
            let mode: ScreenMode = match self.control.name_tbl_arrang() {
                0 => {
                    // 单屏
                    ScreenMode::OneScreenL
                }
                1 => {
                    // 单屏
                    ScreenMode::OneScreenH
                }
                2 => {
                    // 横版
                    ScreenMode::HeriMirror
                }
                3 => {
                    // 竖版
                    ScreenMode::VerMirror
                }
                _ => unimplemented!(),
            };
            self.base.doing_remap_name_table(mode);
        }

        fn remapping_chr_rom(&mut self, prg_bank: u8) {
            match self.control.prg_mode() {
                0 | 1 => self.base.remapping_cpu(
                    |prg_rom: &mut [Rc<RefCell<Vec<u8>>>; 4],
                     prg_buf_8k: &Vec<Rc<RefCell<Vec<u8>>>>| {
                        // 32KB 模式
                        let bank_num = ((prg_bank & 0xe) * 2) as usize;
                        for i in 0..4 {
                            prg_rom[i] = prg_buf_8k[bank_num + i].clone();
                        }
                        println!("switch: PRG-ROM 32KB to:{}\n", bank_num);
                    },
                ),
                2 => self.base.remapping_cpu(
                    |prg_rom: &mut [Rc<RefCell<Vec<u8>>>; 4],
                     prg_buf_8k: &Vec<Rc<RefCell<Vec<u8>>>>| {
                        // 固定低16KB到最后 切换高16KB
                        let bank_num = ((prg_bank & 0xf) * 2) as usize;
                        prg_rom[0] = prg_buf_8k[0].clone();
                        prg_rom[1] = prg_buf_8k[1].clone();
                        prg_rom[2] = prg_buf_8k[bank_num].clone();
                        prg_rom[3] = prg_buf_8k[bank_num + 1].clone();
                        println!("switch: Hi PRG-ROM 16KB to:{}\n", bank_num);
                    },
                ),
                3 => self.base.remapping_cpu(
                    |prg_rom: &mut [Rc<RefCell<Vec<u8>>>; 4],
                     prg_buf_8k: &Vec<Rc<RefCell<Vec<u8>>>>| {
                        // 固定高16KB到最后 切换低16KB
                        let bank_num = ((prg_bank & 0xf) * 2) as usize;
                        let last = prg_buf_8k.len() - 1;
                        println!("bank_num={bank_num},last={last}");
                        assert!(bank_num <= last);
                        // if bank_num > last {return;}
                        prg_rom[0] = prg_buf_8k[bank_num].clone();
                        prg_rom[1] = prg_buf_8k[bank_num + 1].clone();
                        prg_rom[2] = prg_buf_8k[last - 1].clone();
                        prg_rom[3] = prg_buf_8k[last].clone();
                        println!("switch: Lo PRG-ROM 16KB to:{}\n", bank_num);
                    },
                ),
                _ => unimplemented!(),
            };
        }

        fn remapping_chr_bank0(&mut self, chr_bank0: u8) {
            if self.control.chr_mode() {
                self.base.remapping_ppu(
                    |ppu_bank: &mut [Rc<RefCell<Vec<u8>>>; 16],
                     pattern_buf_1k: &Vec<Rc<RefCell<Vec<u8>>>>| {
                        let bank_num = ((chr_bank0 & 0xf) * 4) as usize;
                        for i in 0..4 {
                            ppu_bank[i] = pattern_buf_1k[bank_num + i].clone();
                        }
                    },
                );
            } else {
                self.base.remapping_ppu(
                    |ppu_bank: &mut [Rc<RefCell<Vec<u8>>>; 16],
                     pattern_buf_1k: &Vec<Rc<RefCell<Vec<u8>>>>| {
                        let bank_num = ((chr_bank0 & 0xe) * 4) as usize;
                        for i in 0..8 {
                            ppu_bank[i] = pattern_buf_1k[bank_num + i].clone();
                        }
                    },
                );
            }
        }

        fn remapping_chr_bank1(&mut self, chr_bank1: u8) {
            if self.control.chr_mode() {
                self.base.remapping_ppu(
                    |ppu_bank: &mut [Rc<RefCell<Vec<u8>>>; 16],
                     pattern_buf_1k: &Vec<Rc<RefCell<Vec<u8>>>>| {
                        let bank_num = ((chr_bank1 & 0xf) * 4) as usize;
                        for i in 0..4 {
                            ppu_bank[i + 4] = pattern_buf_1k[bank_num + i].clone();
                        }
                    },
                );
            }
        }
    }

    impl MapperFunc for MapperMmc1 {
        fn new(base: MapperBase) -> Self {
            MapperMmc1 {
                base,
                control: CtrlReg(0x0c),
                shifter: 0x10,
            }
        }

        fn reset(&mut self) {
            self.shifter = 0x10;
            self.control.set_prg_mode(3);
            self.remapping_chr_rom(0);
            self.remapping_chr_bank0(0);
            self.remapping_chr_bank1(0);
        }
    }

    impl Bus for MapperMmc1 {
        fn write(&mut self, addr: u16, val: u8) {
            println!("addr:{:x},val:{:x}", addr, val);

            if (val & 0x80) == 0x80 {
                self.shifter = 0x10;
                self.control.set_prg_mode(3);
                self.remapping_name_table();
            } else {
                let last = (self.shifter & 0x01) == 0x01;
                self.shifter = (self.shifter >> 1) | ((val & 0x1) << 4);
                if last {
                    match (addr >> 13) & 0x3 {
                        0 => {
                            /*  Control (internal, $8000-$9FFF)
                                4bit0
                                -----
                                CPPMM
                                |||||
                                |||++- Nametable arrangement: (0: one-screen, lower bank; 1: one-screen, upper bank;
                                |||               2: horizontal arrangement ("vertical mirroring", PPU A10);
                                |||               3: vertical arrangement ("horizontal mirroring", PPU A11) )
                                |++--- PRG ROM bank mode (0, 1: switch 32 KB at $8000, ignoring low bit of bank number;
                                |                         2: fix first bank at $8000 and switch 16 KB bank at $C000;
                                |                         3: fix last bank at $C000 and switch 16 KB bank at $8000)
                                +----- CHR ROM bank mode (0: switch 8 KB at a time; 1: switch two separate 4 KB banks)
                            */
                            let name_tbl_arrang = self.control.name_tbl_arrang();
                            self.control.0 = self.shifter;
                            if name_tbl_arrang != (self.shifter & 0x3) {
                                self.remapping_name_table();
                            }
                        }
                        1 => {
                            /*
                               CHR bank 0 (internal, $A000-$BFFF)
                               4bit0
                               -----
                               CCCCC
                               |||||
                               +++++- Select 4 KB or 8 KB CHR bank at PPU $0000 (low bit ignored in 8 KB mode)
                            */
                            self.remapping_chr_bank0(self.shifter);
                        }
                        2 => {
                            /*
                                CHR bank 1 (internal, $C000-$DFFF)
                                4bit0
                                -----
                                CCCCC
                                |||||
                                +++++- Select 4 KB CHR bank at PPU $1000 (ignored in 8 KB mode)
                            */
                            self.remapping_chr_bank1(self.shifter);
                        }
                        3 => {
                            /*
                               PRG bank (internal, $E000-$FFFF)
                               4bit0
                               -----
                               RPPPP
                               |||||
                               |++++- Select 16 KB PRG ROM bank (low bit ignored in 32 KB mode)
                               +----- MMC1B and later: PRG RAM chip enable (0: enabled; 1: disabled; ignored on MMC1A)
                                   MMC1A: Bit 3 bypasses fixed bank logic in 16K mode (0: fixed bank affects A17-A14;
                                   1: fixed bank affects A16-A14 and bit 3 directly controls A17)
                            */
                            self.remapping_chr_rom(self.shifter);
                        }
                        _ => unreachable!(),
                    };
                    self.shifter = 0x10;
                }
            }
        }
    }
}
