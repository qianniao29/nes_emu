
pub mod mapper00{
    use std::cell::RefCell;
    use std::rc::Rc;
    pub use nes_emu::mapper::Mapper;
 
    impl MapperFunc for Mapper{
        fn reset()->i32{
            0
        }
    }
    fn mapping_ppu_mem(pattern_buff1k:&Vec<Rc<RefCell<Vec<u8>>>>) -> [Rc<RefCell<Vec<u8>>>; 16]
    {
        let mut ppu_bank: [Rc<RefCell<Vec<u8>>>; 16]=Default::default();
        for i in 0..8 {
            ppu_bank[i] = pattern_buff1k[i].clone();
        }
        ppu_bank
    }

    pub fn new_mapper00() -> Mapper{
        Mapper{
            reset_func: reset,
            mapping_ppu_mem_func:mapping_ppu_mem,
        }
    }
}