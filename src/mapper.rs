pub mod mapper_mmc1;
pub mod mapper_nrom;

pub mod mapper {
    use super::{mapper_mmc1::mapper_mmc1::MapperMmc1, mapper_nrom::mapper_nrom::MapperNrom};
    use crate::common::{Bus, Remap};
    use std::{cell::RefCell, rc::Rc};

    pub enum MapperX {
        Mapper0(MapperNrom),
        Mapper1(MapperMmc1),
    }

    impl MapperX {
        pub fn new(
            map_id: u8,
            four_screen: bool,
            mirror_flag: bool,
            prgrom_size_16k: u8,
            cpu_mem: *mut dyn for<'a> Remap<'a, [Rc<RefCell<Vec<u8>>>; 4]>,
            ppu_mem: *mut dyn for<'a> Remap<'a, [Rc<RefCell<Vec<u8>>>; 16]>,
            pattern_buf_1k: Vec<Rc<RefCell<Vec<u8>>>>,
            prg_buf_8k: Vec<Rc<RefCell<Vec<u8>>>>,
        ) -> MapperX {
            let base = MapperBase::new(
                four_screen,
                mirror_flag,
                prgrom_size_16k,
                cpu_mem,
                ppu_mem,
                pattern_buf_1k,
                prg_buf_8k,
            );
            match map_id {
                0 => MapperX::Mapper0(MapperNrom::new(base)),
                1 => MapperX::Mapper1(MapperMmc1::new(base)),
                _ => unimplemented!("Unsupported mapper."),
            }
        }

        pub fn reset(&mut self) {
            let base = match self {
                MapperX::Mapper0(nrom) => {
                    nrom.reset();
                    &mut nrom.base
                }
                MapperX::Mapper1(mmc1) => {
                    mmc1.reset();
                    &mut mmc1.base
                }
            };
            base.reset_name_table();
        }

        pub fn mapper_base(&mut self) -> &mut MapperBase {
            match self {
                MapperX::Mapper0(nrom) => &mut nrom.base,
                MapperX::Mapper1(mmc1) => &mut mmc1.base,
            }
        }

        pub fn get_bus(&mut self) -> *mut dyn Bus {
            match self {
                MapperX::Mapper0(nrom) => nrom,
                MapperX::Mapper1(mmc1) => mmc1,
            }
        }
    }

    pub enum ScreenMode {
        OneScreenL,
        OneScreenH,
        VerMirror,
        HeriMirror,
        FourScreen,
    }
    pub struct MapperBase {
        pub four_screen: bool,
        pub mirror_flag: bool,
        pub prgrom_size_16k: u8,
        vram: [Rc<RefCell<Vec<u8>>>; 2],
        pub exvram: Vec<Rc<RefCell<Vec<u8>>>>,
        pub pattern_buf_1k: Vec<Rc<RefCell<Vec<u8>>>>,
        pub prg_buf_8k: Vec<Rc<RefCell<Vec<u8>>>>,
        pub remap_cpu_mem: *mut dyn for<'a> Remap<'a, [Rc<RefCell<Vec<u8>>>; 4]>,
        pub remap_ppu_mem: *mut dyn for<'a> Remap<'a, [Rc<RefCell<Vec<u8>>>; 16]>,
    }

    impl MapperBase {
        pub fn new(
            four_screen: bool,
            mirror_flag: bool,
            prgrom_size_16k: u8,
            cpu_mem: *mut dyn for<'a> Remap<'a, [Rc<RefCell<Vec<u8>>>; 4]>,
            ppu_mem: *mut dyn for<'a> Remap<'a, [Rc<RefCell<Vec<u8>>>; 16]>,
            pattern_buf_1k: Vec<Rc<RefCell<Vec<u8>>>>,
            prg_buf_8k: Vec<Rc<RefCell<Vec<u8>>>>,
        ) -> Self {
            MapperBase {
                four_screen,
                mirror_flag,
                prgrom_size_16k,
                vram: [
                    Rc::new(RefCell::new(vec![0_u8; 1024])),
                    Rc::new(RefCell::new(vec![0_u8; 1024])),
                ],
                exvram: Default::default(),
                pattern_buf_1k,
                prg_buf_8k,
                remap_cpu_mem: cpu_mem,
                remap_ppu_mem: ppu_mem,
            }
        }

        pub fn remapping_cpu<F>(&mut self, mut _doing_remmap: F)
        where
            F: FnMut(&mut [Rc<RefCell<Vec<u8>>>; 4], &Vec<Rc<RefCell<Vec<u8>>>>),
        {
            _doing_remmap(
                unsafe { (*self.remap_cpu_mem).get_remap_mem() },
                &self.prg_buf_8k,
            );
        }

        pub fn remapping_ppu<F>(&mut self, mut _doing_remmap: F)
        where
            F: FnMut(&mut [Rc<RefCell<Vec<u8>>>; 16], &Vec<Rc<RefCell<Vec<u8>>>>),
        {
            _doing_remmap(
                unsafe { (*self.remap_ppu_mem).get_remap_mem() },
                &self.pattern_buf_1k,
            );
        }

        pub fn doing_remap_name_table(&mut self, mode: ScreenMode) {
            match mode {
                ScreenMode::OneScreenL => {
                    // 单屏
                    let vram = self.vram[0].clone();
                    self.remapping_ppu(|ppu_bank: &mut [Rc<RefCell<Vec<u8>>>; 16], _| {
                        for i in 8..12 {
                            ppu_bank[i] = vram.clone();
                        }
                    })
                }
                ScreenMode::OneScreenH => {
                    // 单屏
                    let vram = self.vram[1].clone();
                    self.remapping_ppu(|ppu_bank: &mut [Rc<RefCell<Vec<u8>>>; 16], _| {
                        for i in 8..12 {
                            ppu_bank[i] = vram.clone();
                        }
                    })
                }
                ScreenMode::HeriMirror => {
                    // 横版
                    let vram0 = self.vram[0].clone();
                    let vram1 = self.vram[1].clone();
                    self.remapping_ppu(|ppu_bank: &mut [Rc<RefCell<Vec<u8>>>; 16], _| {
                        ppu_bank[8] = vram0.clone();
                        ppu_bank[9] = vram1.clone();
                        ppu_bank[10] = vram0.clone();
                        ppu_bank[11] = vram1.clone();
                    })
                }
                ScreenMode::VerMirror => {
                    // 竖版
                    let vram0 = self.vram[0].clone();
                    let vram1 = self.vram[1].clone();
                    self.remapping_ppu(|ppu_bank: &mut [Rc<RefCell<Vec<u8>>>; 16], _| {
                        ppu_bank[8] = vram0.clone();
                        ppu_bank[9] = vram0.clone();
                        ppu_bank[10] = vram1.clone();
                        ppu_bank[11] = vram1.clone();
                    })
                }
                ScreenMode::FourScreen => {
                    let vram0 = self.vram[0].clone();
                    let vram1 = self.vram[1].clone();
                    let exvram0 = self.exvram[0].clone();
                    let exvram1 = self.exvram[1].clone();

                    self.remapping_ppu(|ppu_bank: &mut [Rc<RefCell<Vec<u8>>>; 16], _| {
                        ppu_bank[8] = vram1.clone();
                        ppu_bank[9] = vram0.clone();
                        ppu_bank[10] = exvram0.clone();
                        ppu_bank[11] = exvram1.clone();
                    })
                }
            };
            self.remapping_ppu(|ppu_bank: &mut [Rc<RefCell<Vec<u8>>>; 16], _| {
                for i in 12..16 {
                    //镜像地址
                    ppu_bank[i] = ppu_bank[i - 4].clone();
                }
            });
        }

        pub fn reset_name_table(&mut self) {
            let mode: ScreenMode = if self.four_screen {
                ScreenMode::FourScreen
            } else if self.mirror_flag {
                ScreenMode::HeriMirror
            } else {
                ScreenMode::VerMirror
            };
            self.doing_remap_name_table(mode);
        }
    }
    pub trait MapperFunc {
        fn new(base: MapperBase) -> Self;
        fn reset(&mut self);
    }
}
