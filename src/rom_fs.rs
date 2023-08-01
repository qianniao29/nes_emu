pub mod rom {
    use bitfield;
    use std::cell::RefCell;
    use std::fs::File;
    use std::io::{BufRead, BufReader, Error, Read, Seek, SeekFrom, Write};
    use std::rc::Rc;

    use crate::common::error;
    /*
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

    /*
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

    /*
    文件头：
     0-3: string    "NES"<EOF>
       4: byte      以 16384(0x4000) 字节作为单位的 PRG-ROM 大小数量
       5: byte      以 8192(0x2000) 字节作为单位的 CHR-ROM 大小数量
       6: bitfield  Flags 6
       7: bitfield  Flags 7
    8-15: byte      保留用，应该为 0.

    CHR-ROM - 角色只读存储器 (用于图像显示)
    */
    #[derive(Debug)]
    pub struct RomHead {
        pub id: [u8; 4],
        pub prgrom_size_16k: u8,
        pub chrrom_size_8k: u8,
        pub flag6: Flag6,
        pub flag7: Flag7,
        reserved: [u8; 8],
    }

    impl RomHead {
        pub fn new(raw: &[u8]) -> Self {
            Self {
                id: [raw[0], raw[1], raw[2], raw[3]],
                prgrom_size_16k: raw[4],
                chrrom_size_8k: raw[5],
                flag6: Flag6(raw[6]),
                flag7: Flag7(raw[7]),
                reserved: [
                    raw[8], raw[9], raw[10], raw[11], raw[12], raw[13], raw[14], raw[15],
                ],
            }
        }
    }

    pub fn read_head(file_name: &String) -> Result<(File, RomHead), Error> {
        let mut rom_file = File::open(file_name)?;
        let mut head_buf: [u8; 16] = [0; 16];
        rom_file.read(&mut head_buf)?;
        let head: RomHead = RomHead::new(&head_buf);

        Ok((rom_file, head))
    }

    pub fn read_prgrom(
        rom_file: &mut File,
        prgrom_size_16k: u8,
        trainer_flag: bool,
    ) -> Result<Vec<u8>, Error> {
        let prgrom_size: u32 = (prgrom_size_16k as u32) * 16 * 1024;

        if trainer_flag {
            rom_file.seek(SeekFrom::Current(512))?;
        }

        let mut prgrom_buf = Vec::new();
        prgrom_buf.resize(prgrom_size as usize, 0);
        rom_file.read(&mut prgrom_buf)?;

        Ok(prgrom_buf)
    }

    pub fn read_chrrom(
        rom_file: &mut File,
        chrrom_size_8k: u8,
    ) -> Result<Vec<Rc<RefCell<Vec<u8>>>>, Error> {
        let mut pattern_buff1k = Vec::new();
        for _ in 0..(chrrom_size_8k * 8) {
            let mut chrrom_buf = Vec::new();
            chrrom_buf.resize(1024, 0);
            rom_file.read(&mut chrrom_buf)?;
            pattern_buff1k.push(Rc::new(RefCell::new(chrrom_buf)));
        }

        Ok(pattern_buff1k)
    }

    pub fn load_rom(
        file_name: &String,
    ) -> Result<(RomHead, Vec<u8>, Vec<Rc<RefCell<Vec<u8>>>>), error::CustomError> {
        let (mut rom_file, head) = read_head(file_name)?;
        let head_id_str = std::str::from_utf8(&head.id[..])?;
        if head_id_str != "NES\u{1a}" {
            println!("Not support ROM type, {:?}", head.id);
        }

        let prgrom_buf = read_prgrom(
            &mut rom_file,
            head.prgrom_size_16k,
            head.flag6.trainer_flag(),
        )?;

        // println!("flag6,{:#x?}", head.flag6.map_lid());
        // println!("flag7,{:#x?}", head.flag7.map_hid());
        // println!("{:#x?}", head);

        let mut pattern_buff1k = Vec::new();
        if head.chrrom_size_8k == 0 {
            // 允许没有 CHR-ROM(使用 CHR-RAM 代替)
            for _ in 0..8 {
                let mut chrrom_buf = Vec::new();
                chrrom_buf.resize(1024, 0);
                pattern_buff1k.push(Rc::new(RefCell::new(chrrom_buf)));
            }
        } else {
            pattern_buff1k = read_chrrom(&mut rom_file, head.chrrom_size_8k)?;
        }
        Ok((head, prgrom_buf, pattern_buff1k))
    }
}
