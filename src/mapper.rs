pub mod mapper_mmc1;
pub mod mapper_nrom;

pub mod mapper {
    use super::{mapper_mmc1::mapper_mmc1::MapperMmc1, mapper_nrom::mapper_nrom::MapperNrom};
    use crate::common::Bus;
    use std::{cell::RefCell, rc::Rc};

    pub enum MapperX {
        Mapper0(MapperNrom),
        Mapper1(MapperMmc1),
    }

    impl Bus for MapperX {
        fn write(&mut self, addr: u16, val: u8) {
            match self {
                MapperX::Mapper0(_nrom) => {}
                MapperX::Mapper1(mmc1) => {
                    mmc1.write(addr, val);
                }
            }
        }
    }

    pub struct Mapper {
        pub bank: [Rc<RefCell<Vec<u8>>>; 16],
        pub mapperx: MapperX,
    }

    impl Mapper {
        pub fn new(mapperx: MapperX) -> Self {
            Mapper {
                bank: Default::default(),
                mapperx,
            }
        }
        pub fn read_bank(&mut self, addr: u16) -> u8 {
            let i: usize = (addr >> 10).into();
            let j: usize = (addr & 0x3ff).into();
            let bank = self.bank[i].clone();
            return bank.borrow()[j];
        }
        pub fn write_bank(&mut self, addr: u16, val: u8) {
            let i: usize = (addr >> 10).into();
            let j: usize = (addr & 0x3ff).into();
            let bank = self.bank[i].clone();
            bank.borrow_mut()[j] = val;
        }
        pub fn mapping_name_table(
            &mut self,
            four_screen: bool,
            mirror_flag: bool,
            pattern_buff1k: &Vec<Rc<RefCell<Vec<u8>>>>,
        ) {
            let vram = vec![
                Rc::new(RefCell::new(vec![0; 1024])),
                Rc::new(RefCell::new(vec![0; 1024])),
            ];

            for i in 0..8 {
                self.bank[i] = pattern_buff1k[i].clone();
            }

            if four_screen {
                // 4 屏
                let exvram = vec![
                    Rc::new(RefCell::new(vec![0; 1024])),
                    Rc::new(RefCell::new(vec![0; 1024])),
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
    pub trait MapperFunc {
        fn new() -> Self;
        fn reset(&self) -> i32 {
            0
        }
    }

    impl Bus for Mapper {
        fn read(&mut self, addr: u16) -> u8 {
            return self.read_bank(addr);
        }

        fn write(&mut self, addr: u16, val: u8) {
            if addr >= 0x2000 && addr < 0x3fff {
                self.write_bank(addr, val);
            } else {
                self.mapperx.write(addr, val);
            }
        }
    }
}
