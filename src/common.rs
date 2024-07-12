use std::usize;

pub mod error {
    use std::fmt::{Display, Formatter};
    use std::io::Error as IoError;
    use std::num::ParseIntError;
    use std::str::Utf8Error;

    #[derive(Debug)]
    pub enum CustomError {
        ParseIntError(std::num::ParseIntError),
        Utf8Error(std::str::Utf8Error),
        IoError(std::io::Error),
    }

    impl std::error::Error for CustomError {
        fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
            match &self {
                CustomError::IoError(ref e) => Some(e),
                CustomError::Utf8Error(ref e) => Some(e),
                CustomError::ParseIntError(ref e) => Some(e),
            }
        }
    }

    impl Display for CustomError {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match &self {
                CustomError::IoError(ref e) => e.fmt(f),
                CustomError::Utf8Error(ref e) => e.fmt(f),
                CustomError::ParseIntError(ref e) => e.fmt(f),
            }
        }
    }

    impl From<ParseIntError> for CustomError {
        fn from(s: std::num::ParseIntError) -> Self {
            CustomError::ParseIntError(s)
        }
    }

    impl From<IoError> for CustomError {
        fn from(s: std::io::Error) -> Self {
            CustomError::IoError(s)
        }
    }

    impl From<Utf8Error> for CustomError {
        fn from(s: std::str::Utf8Error) -> Self {
            CustomError::Utf8Error(s)
        }
    }
}

pub trait Bus {
    fn read(&mut self, _addr: u16) -> u8 {
        0
    }
    fn write(&mut self, _addr: u16, _val: u8) {}
    fn dma_write(&mut self, _addr: u16, _src: &[u8], _len: usize) {}
    fn push(&mut self, _sp_piont: &mut u8, _data: u8) {}
    fn pop(&self, _sp_piont: &mut u8) -> u8 {
        0
    }
}

pub trait Irq {
    fn nmi_handler(&mut self) {}
    fn irq_handler(&mut self) {}
    fn irq_req(&mut self) {}
    fn irq_ack(&mut self) {}
}
