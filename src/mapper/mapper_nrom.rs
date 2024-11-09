pub mod mapper_nrom {
    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::{
        common::Bus,
        mapper::mapper::{MapperBase, MapperFunc},
    };

    pub struct MapperNrom {
        pub base: MapperBase,
    }

    impl MapperFunc for MapperNrom {
        fn new(base: MapperBase) -> Self {
            MapperNrom { base }
        }

        fn reset(&mut self) {
            let exvram = vec![
                Rc::new(RefCell::new(vec![0; 1024])),
                Rc::new(RefCell::new(vec![0; 1024])),
            ];
            self.base.exvram.resize(2, Default::default());
            self.base.exvram[0] = exvram[0].clone();
            self.base.exvram[1] = exvram[1].clone();

            let prgrom_size_16k = self.base.prgrom_size_16k;
            self.base.remapping_cpu(
                |prg_rom: &mut [Rc<RefCell<Vec<u8>>>; 4],
                 prg_buf_8k: &Vec<Rc<RefCell<Vec<u8>>>>| {
                    let mut offset = 0;
                    if prgrom_size_16k > 1 {
                        offset = 2;
                    }
                    prg_rom[0] = prg_buf_8k[0].clone();
                    prg_rom[1] = prg_buf_8k[1].clone();
                    prg_rom[2] = prg_buf_8k[offset].clone();
                    prg_rom[3] = prg_buf_8k[offset + 1].clone();
                },
            );
            self.base.remapping_ppu(
                |ppu_bank: &mut [Rc<RefCell<Vec<u8>>>; 16],
                 pattern_buf_1k: &Vec<Rc<RefCell<Vec<u8>>>>| {
                    for i in 0..8 {
                        ppu_bank[i] = pattern_buf_1k[i].clone();
                    }
                },
            );
        }
    }

    impl Bus for MapperNrom {}
}
