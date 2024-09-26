pub mod mapper_mmc1 {
    use bitfield::BitMut;

    use crate::{
        common::Bus,
        mapper::mapper::{Mapper, MapperFunc},
    };
    use std::cell::RefCell;
    use std::rc::Rc;

    /**************************************************************************************
     * CPU $6000-$7FFF: 8 KB PRG RAM bank, (optional)
     * CPU $8000-$BFFF: 16 KB PRG ROM bank, either switchable or fixed to the first bank
     * CPU $C000-$FFFF: 16 KB PRG ROM bank, either fixed to the last bank or switchable
     * PPU $0000-$0FFF: 4 KB switchable CHR bank
     * PPU $1000-$1FFF: 4 KB switchable CHR bank
     **************************************************************************************/

    pub struct MapperMmc1 {
        control: u8,
        chr_bank0: u8,
        chr_bank1: u8,
        prg_bank: u8,
        shifter: u8,
        wcnt: u8,
    }

    impl MapperFunc for MapperMmc1 {
        fn new() -> Self {
            MapperMmc1 {
                control: 0,
                chr_bank0: 0,
                chr_bank1: 0,
                prg_bank: 0,
                shifter: 0,
                wcnt: 0,
            }
        }
    }

    impl Bus for MapperMmc1 {
        fn write(&mut self, addr: u16, val: u8) {
            if (val & 0x80) == 0x80 {
                self.shifter = 0;
            } else {
                self.shifter.set_bit(self.wcnt as usize, (val & 0x1) == 0x1);
                self.wcnt += 1;
                if self.wcnt == 5 {
                    self.wcnt = 0;
                    match (addr >> 13) & 0x3 {
                        /*
                            Control (internal, $8000-$9FFF)
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
                        0 => self.control = self.shifter,
                        1 => self.chr_bank0 = self.shifter,
                        2 => self.chr_bank1 = self.shifter,
                        3 => self.prg_bank = self.shifter,
                        _ => unreachable!(),
                    };
                    self.shifter = 0;
                }
            }
        }
    }
}
