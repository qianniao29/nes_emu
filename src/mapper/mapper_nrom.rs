pub mod mapper_nrom {
    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::{
        common::Bus,
        mapper::mapper::{Mapper, MapperFunc},
    };

    pub struct MapperNrom {}

    impl MapperFunc for MapperNrom {
        fn new() -> Self {
            MapperNrom {}
        }
    }
}
