pub mod rom {
    use bitfield;
    use std::cell::RefCell;
    use std::fs::File;
    use std::io::{Error, Read, Seek, SeekFrom};
    use std::rc::Rc;

    /* byte 6
    Flags 6:
           D~7654 3210
             ---------
             NNNN FTBM
             |||| |||+-- Hard-wired nametable mirroring type
             |||| |||     0: Horizontal (vertical arrangement) or mapper-controlled
             |||| |||     1: Vertical (horizontal arrangement)
             |||| ||+--- "Battery" and other non-volatile memory
             |||| ||      0: Not present
             |||| ||      1: Present
             |||| |+--- 512-byte Trainer
             |||| |      0: Not present
             |||| |      1: Present between Header and PRG-ROM data
             |||| +---- Hard-wired four-screen mode
             ||||        0: No
             ||||        1: Yes
             ++++------ Mapper Number D0..D3
    */
    bitfield! {
        pub struct Flag6(u8);
        impl Debug;
        u8;
        pub mirror_flag,  _: 0;
        pub battery_flag, _: 1;
        pub trainer_flag, _: 2;
        pub four_screen,  _: 3;
        pub map_lid,      _: 7, 4;
    }

    /* byte 7
    Flags 7:
           D~7654 3210
             ---------
             NNNN 10TT
             |||| ||++- Console type
             |||| ||     0: Nintendo Entertainment System/Family Computer
             |||| ||     1: Nintendo Vs. System
             |||| ||     2: Nintendo Playchoice 10
             |||| ||     3: Extended Console Type
             |||| ++--- NES 2.0 identifier
             ++++------ Mapper Number D4..D7
     */
    bitfield! {
        pub struct Flag7(u8);
        impl Debug;
        u8;
        pub vs_flag,   _: 0;
        pub pc10_flag, _: 1;
        pub id,        _: 3, 2;
        pub map_hid,   _: 7, 4;
    }

    /* byte 8
    Mapper MSB/Submapper    use std::{cell::RefCell, rc::Rc};

       D~7654 3210
         ---------
         SSSS NNNN
         |||| ++++- Mapper number D8..D11
         ++++------ Submapper number

     */
    bitfield! {
        pub struct MapperExt(u8);
        impl Debug;
        u8;
        pub map_uid,   _: 3, 0; //Mapper number D8..D11
        pub map_sub,   _: 7, 4; //Submapper number
    }

    /* byte 9
    PRG-ROM/CHR-ROM size MSB
    D~7654 3210
        ---------
        CCCC PPPP
        |||| ++++- PRG-ROM size MSB
        ++++------ CHR-ROM size MSB
    */
    bitfield! {
        pub struct RomSize(u8);
        impl Debug;
        u8;
        pub prg,   _: 3, 0; //PRG-ROM size MSB
        pub chr,   _: 7, 4; //CHR-ROM size MSB
    }

    /* byte 10
    PRG-RAM/EEPROM size
    D~7654 3210
         ---------
         pppp PPPP
         |||| ++++- PRG-RAM (volatile) shift count
         ++++------ PRG-NVRAM/EEPROM (non-volatile) shift count
       If the shift count is zero, there is no PRG-(NV)RAM.
       If the shift count is non-zero, the actual size is
       "64 << shift count" bytes, i.e. 8192 bytes for a shift count of 7.

     */
    /* byte 11
    CHR-RAM size
       D~7654 3210
         ---------
         cccc CCCC
         |||| ++++- CHR-RAM size (volatile) shift count
         ++++------ CHR-NVRAM size (non-volatile) shift count
       If the shift count is zero, there is no CHR-(NV)RAM.
       If the shift count is non-zero, the actual size is
       "64 << shift count" bytes, i.e. 8192 bytes for a shift count of 7.
     */
    bitfield! {
        pub struct RamSize(u8);
        impl Debug;
        u8;
        pub ram,   _: 3, 0; //PRG-RAM (volatile) shift count
        pub nvram,   _: 7, 4; //PRG-NVRAM/EEPROM (non-volatile) shift count
    }

    /* byte 12
    CPU/PPU Timing
       D~7654 3210
         ---------
         .... ..VV
                ++- CPU/PPU timing mode
                     0: RP2C02 ("NTSC NES")
                     1: RP2C07 ("Licensed PAL NES")
                     2: Multiple-region
                     3: UA6538 ("Dendy")
     */

    /* byte 13
    When Byte 7 AND 3 =1: Vs. System Type
       D~7654 3210
         ---------
         MMMM PPPP
         |||| ++++- Vs. PPU Type
         ++++------ Vs. Hardware Type

       When Byte 7 AND 3 =3: Extended Console Type
       D~7654 3210
         ---------
         .... CCCC
              ++++- Extended Console Type
     */
    bitfield! {
        pub struct SysType(u8);
        impl Debug;
        u8;
        pub ppu_console_type,   _: 3, 0; //PPU Type/Extended Console Type
        pub hardware_type,   _: 7, 4; //Hardware Type
    }

    /* byte 14
    Miscellaneous ROMs
           D~7654 3210
             ---------
             .... ..RR
                    ++- Number of miscellaneous ROMs present
    */

    /* byte 15
     Default Expansion Device
           D~7654 3210
             ---------
             ..DD DDDD
               ++-++++- Default Expansion Device
    */

    /*
    NES2.0 文件头：
     0-3: string    "NES"<EOF>
       4: byte      以 16384(0x4000) 字节作为单位的 PRG-ROM 大小数量
       5: byte      以 8192(0x2000) 字节作为单位的 CHR-ROM 大小数量
       6: bitfield  Flags 6
       7: bitfield  Flags 7
       8: bitfield  Mapper MSB/Submapper
       9: bitfield  PRG-ROM/CHR-ROM size MSB
       10: bitfield  PRG-RAM/EEPROM size
       11: bitfield  CHR-RAM size
       12: byte  CPU/PPU Timing, bit0~bit1
       13: bitfield  System Type
       14: byte  Miscellaneous ROMs, bit0~bit1
       15: byte  Expansion Device, bit0~bit5

    CHR-ROM - 角色只读存储器 (用于图像显示)
    */
    #[derive(Debug)]
    pub struct RomHead {
        pub id: [u8; 4],
        pub prgrom_size_16k: u8,
        pub chrrom_size_8k: u8,
        pub flag6: Flag6,
        pub flag7: Flag7,
        pub map_ext: MapperExt,
        pub rom_size: RomSize,
        pub prg_ram_size: RamSize,
        pub chr_ram_size: RamSize,
        pub timing: u8,
        pub sys_type: SysType,
        pub misc_rom: u8,
        pub exp_dev: u8,
    }

    impl Default for RomHead {
        fn default() -> Self {
            Self {
                id: [0; 4],
                prgrom_size_16k: 0,
                chrrom_size_8k: 0,
                flag6: Flag6(0),
                flag7: Flag7(0),
                map_ext: MapperExt(0),
                rom_size: RomSize(0),
                prg_ram_size: RamSize(0),
                chr_ram_size: RamSize(0),
                timing: 0,
                sys_type: SysType(0),
                misc_rom: 0,
                exp_dev: 0,
            }
        }
    }

    impl RomHead {
        fn init(&mut self, raw: &[u8]) {
            self.id = [raw[0], raw[1], raw[2], raw[3]];
            self.prgrom_size_16k = raw[4];
            self.chrrom_size_8k = raw[5];
            self.flag6 = Flag6(raw[6]);
            self.flag7 = Flag7(raw[7]);
            self.map_ext = MapperExt(raw[8]);
            self.rom_size = RomSize(raw[9]);
            self.prg_ram_size = RamSize(raw[10]);
            self.chr_ram_size = RamSize(raw[11]);
            self.timing = raw[12];
            self.sys_type = SysType(raw[13]);
            self.misc_rom = raw[14];
            self.exp_dev = raw[15];
        }

        fn read_head(&mut self, file_name: &String) -> Result<File, Error> {
            let mut rom_file = File::open(file_name)?;
            let mut head_buf: [u8; 16] = [0; 16];
            rom_file.read(&mut head_buf)?;
            self.init(&head_buf);

            Ok(rom_file)
        }
    }
    pub struct Rom {
        pub rom_head: RomHead,
    }

    impl Rom {
        pub fn new() -> Self {
            Rom {
                rom_head: Default::default(),
            }
        }

        fn read_prgrom(
            rom_file: &mut File,
            prgrom_size_16k: u8,
            trainer_flag: bool,
        ) -> Result<Vec<Rc<RefCell<Vec<u8>>>>, Error> {
            if trainer_flag {
                rom_file.seek(SeekFrom::Current(512))?;
            }

            let mut prgrom_buf_8k = Vec::new();
            for _ in 0..(prgrom_size_16k * 2) {
                let mut buf = Vec::new();
                buf.resize(8192, 0);
                rom_file.read(&mut buf)?;
                prgrom_buf_8k.push(Rc::new(RefCell::new(buf)));
            }

            Ok(prgrom_buf_8k)
        }

        fn read_chrrom(
            rom_file: &mut File,
            chrrom_size_8k: u8,
        ) -> Result<Vec<Rc<RefCell<Vec<u8>>>>, Error> {
            let mut pattern_buf_1k = Vec::new();
            for _ in 0..(chrrom_size_8k * 8) {
                let mut chrrom_buf = Vec::new();
                chrrom_buf.resize(1024, 0);
                rom_file.read(&mut chrrom_buf)?;
                pattern_buf_1k.push(Rc::new(RefCell::new(chrrom_buf)));
            }

            Ok(pattern_buf_1k)
        }

        pub fn load(
            &mut self,
            file_name: &String,
        ) -> Result<(Vec<Rc<RefCell<Vec<u8>>>>, Vec<Rc<RefCell<Vec<u8>>>>), Error> {
            let mut rom_file = self.rom_head.read_head(file_name)?;
            let head_id_str = std::str::from_utf8(&self.rom_head.id[..]).unwrap();
            if head_id_str != "NES\u{1a}" {
                println!("Not support ROM type, {:?}", self.rom_head.id);
            }

            let prg_buf_8k = Self::read_prgrom(
                &mut rom_file,
                self.rom_head.prgrom_size_16k,
                self.rom_head.flag6.trainer_flag(),
            )?;

            let mut pattern_buf_1k = Vec::new();
            if self.rom_head.chrrom_size_8k == 0 {
                // 允许没有 CHR-ROM(使用 CHR-RAM 代替)
                for _ in 0..8 {
                    let mut chrrom_buf = Vec::new();
                    chrrom_buf.resize(1024, 0);
                    pattern_buf_1k.push(Rc::new(RefCell::new(chrrom_buf)));
                }
            } else {
                pattern_buf_1k = Self::read_chrrom(&mut rom_file, self.rom_head.chrrom_size_8k)?;
            }

            Ok((prg_buf_8k, pattern_buf_1k))
        }
    }
}
