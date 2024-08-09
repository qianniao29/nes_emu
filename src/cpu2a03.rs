#![allow(dead_code)]

pub mod cycle {
    static mut CPU_CYCLES: u16 = 0;

    pub fn cpu_cycles_reset() {
        unsafe {
            CPU_CYCLES = 0;
        }
    }

    pub fn cpu_cycles_add(cnt: u16) {
        unsafe {
            CPU_CYCLES = CPU_CYCLES.wrapping_add(cnt);
        }
    }

    pub fn get_cpu_cycles() -> u16 {
        unsafe { CPU_CYCLES }
    }
}

pub mod memory {
    use crate::common::{Bus, Irq};
    use crate::cpu2a03::apu;
    use crate::cpu2a03::cycle::{cpu_cycles_add, get_cpu_cycles};
    use crate::ppu2c02::ppu;

    /*
    address         size 	  describe
    $0000–$07FF 	$0800 	2 KB internal RAM
    $0800–$0FFF 	$0800 	Mirrors of $0000–$07FF
    $1000–$17FF 	$0800 	Mirrors of $0000–$07FF
    $1800–$1FFF 	$0800 	Mirrors of $0000–$07FF
    $2000–$2007 	$0008 	NES PPU registers
    $2008–$3FFF 	$1FF8 	Mirrors of $2000–$2007 (repeats every 8 bytes)
    $4000–$4017 	$0018 	NES APU and I/O registers
    $4018–$401F 	$0008 	APU and I/O functionality that is normally disabled. See CPU Test Mode.
    $4020–$FFFF 	$BFE0 	Cartridge space: PRG ROM, PRG RAM, and mapper registers

    $4020           $1FDF 		Expansion ROM
    $6000           $2000 		SRAM
    $8000           $4000 		PRG-ROM
    $C000           $4000 		PRG-ROM
    */

    pub struct MemMap {
        pub ram: [u8; 0x800],
        pub sram: [u8; 0x2000], // save RAM
        pub prg_rom: [&'static [u8]; 4],
        pub key: [[u8; 8]; 2],
        pub key_indx: [usize; 2],
        pub bus_to_apu: *mut dyn Bus,
        pub bus_to_ppu: *mut dyn Bus,
    }

    impl Bus for MemMap {
        fn read(&mut self, addr: u16) -> u8 {
            let i: usize = addr.into();
            let j: usize = i >> 13;

            if addr & 0x8000 == 0x8000 {
                // 高一位为 1, [$8000, $ffff) 程序 PRG-ROM 区
                self.prg_rom[j & 3][i & 0x1fff]
            } else {
                match j {
                    // 高三位为 0: [$0000, $1fff): 系统主内存，4 次镜像
                    0 => self.ram[i & 0x7ff],
                    // 高三位为 1, [$2000, $3fff): PPU 寄存器，8 字节步进镜像
                    1 => self.ppu_read(addr),
                    // 高三位为 2, [$4000, $5fff): pAPU 寄存器 扩展 ROM 区
                    2 => {
                        if addr == 0x4016 {
                            let val = self.key[0][self.key_indx[0]];
                            self.key_indx[0] += 1;
                            if self.key_indx[0] > 7 {
                                self.key_indx[0] = 0;
                            }
                            val
                        } else if addr == 0x4017 {
                            let val = self.key[1][self.key_indx[1]];
                            self.key_indx[1] += 1;
                            if self.key_indx[1] > 7 {
                                self.key_indx[1] = 0;
                            }
                            val
                        } else if addr == 0x4015 {
                            self.apu_read(addr)
                        } else {
                            0
                        }
                    }
                    // 高三位为 3, [$6000, $7fff): 存档 SRAM 区
                    3 => self.sram[i & 0x1fff],
                    _ => unreachable!("Out of memory range!"),
                }
            }
        }

        fn write(&mut self, addr: u16, val: u8) {
            let i: usize = addr.into();
            let j: usize = i >> 13;
            let bus_to_ppu = self.bus_to_ppu;

            let dma_write = |addr_dma: u16| {
                let offset: usize = (addr_dma & 0x1f00).into();
                let base = addr_dma >> 13;
                let src = match base {
                    0 => {
                        let mirror = offset & 0x7ff;
                        &self.ram[mirror..mirror + 256]
                    }
                    3 => &self.sram[offset..offset + 256],
                    4 | 5 | 6 | 7 => &self.prg_rom[base as usize - 4][offset..256],
                    1 | 2 => {
                        unimplemented!("Can't be operating by DMA.");
                    }
                    _ => unreachable!("Out of memory range!"),
                };
                unsafe {
                    (*bus_to_ppu).dma_write(0, src, 255);
                }
                cpu_cycles_add(513);
                if get_cpu_cycles() & 0x1 == 0x1 {
                    cpu_cycles_add(1);
                }
            };

            match j {
                0 => {
                    self.ram[i & 0x7ff] = val;
                }
                1 => {
                    self.ppu_write(addr, val);
                }
                2 => {
                    if addr == 0x4014 {
                        //DMA write
                        dma_write((val as u16) << 8);
                    } else if addr == 0x4016 {
                        if val & 0x1 == 0 {
                            self.key_indx[0] = 0;
                            self.key_indx[1] = 0;
                        }
                    } else if addr < 0x4020 {
                        self.apu_write(addr, val);
                    }
                }
                3 => {
                    self.sram[i & 0x1fff] = val;
                }
                4 | 5 | 6 | 7 => {
                    unimplemented!("Rom can't be written.");
                }
                _ => unreachable!("Out of memory range!"),
            }
        }

        fn push(&mut self, sp_piont: &mut u8, data: u8) {
            self.ram[0x100 + *sp_piont as usize] = data;
            *sp_piont = (*sp_piont).wrapping_sub(1);
        }

        fn pop(&self, sp_piont: &mut u8) -> u8 {
            *sp_piont = (*sp_piont).wrapping_add(1);
            self.ram[0x100 + *sp_piont as usize]
        }
    }

    impl MemMap {
        pub fn new(apu: *mut dyn Bus, ppu: *mut dyn Bus) -> MemMap {
            MemMap {
                ram: [0; 0x800],
                key: [[0; 8]; 2],
                key_indx: [0; 2],
                sram: [0; 0x2000],
                prg_rom: Default::default(),
                bus_to_apu: apu,
                bus_to_ppu: ppu,
            }
        }

        fn ppu_read(&self, addr: u16) -> u8 {
            unsafe { (*self.bus_to_ppu).read(addr) }
        }

        fn apu_read(&self, addr: u16) -> u8 {
            unsafe { (*self.bus_to_apu).read(addr) }
        }

        fn ppu_dma_write(&mut self, addr: u16, src: &[u8], len: usize) {
            unsafe {
                (*self.bus_to_ppu).dma_write(addr, src, len);
            }
        }
        fn apu_write(&mut self, addr: u16, val: u8) {
            unsafe { (*self.bus_to_apu).write(addr, val) };
        }
        fn ppu_write(&mut self, addr: u16, val: u8) {
            unsafe { (*self.bus_to_ppu).write(addr, val) };
        }
    }
}

pub mod cpu {
    use crate::common::{Bus, Irq};
    use crate::cpu2a03::cycle::{cpu_cycles_add, get_cpu_cycles};
    use crate::cpu2a03::memory::MemMap;
    use bitfield;
    use bitflags::bitflags;
    use std::fmt;

    pub const NMI_VECT_ADDR: u16 = 0xFFFA;
    pub const REST_VECT_ADDR: u16 = 0xFFFC;
    pub const BRK_VECT_ADDR: u16 = 0xFFFE;
    pub const IRQ_VECT_ADDR: u16 = 0xFFFE;

    pub const KEY_A: usize = 0;
    pub const KEY_B: usize = 1;
    pub const KEY_SELECT: usize = 2;
    pub const KEY_START: usize = 3;
    pub const KEY_UP: usize = 4;
    pub const KEY_DOWN: usize = 5;
    pub const KEY_LEFT: usize = 6;
    pub const KEY_RIGHT: usize = 7;

    bitfield! {
        pub struct ProcessorStatus(u8);
        impl Debug;
        u8;
        c, set_c: 0; //Carry flag
        z, set_z: 1; //Zero flag
        i, set_i: 2; //Interrupt disable flag
        d, set_d: 3; //Decimal mode flag
        b, set_b: 4; //Break flag
        _r,set_r: 5; //unused flag, always 1.
        v, set_v: 6; //Overflow flag
        n, set_n: 7; //Negative flag
    }

    impl ProcessorStatus {
        pub fn check_set_c(&mut self, v: u8) {
            if v != 0 {
                self.set_c(true);
            } else {
                self.set_c(false);
            }
        }

        pub fn check_set_z(&mut self, v: u8) {
            if v == 0 {
                self.set_z(true);
            } else {
                self.set_z(false);
            }
        }

        pub fn check_set_v(&mut self, v: u8) {
            if v != 0 {
                self.set_v(true);
            } else {
                self.set_v(false);
            }
        }

        pub fn check_set_n(&mut self, v: u8) {
            if v & 0x80 == 0x80 {
                // v < 0
                self.set_n(true);
            } else {
                self.set_n(false);
            }
        }
    }

    bitflags! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct ProcessorStatusFlags: u8 {
            const FLAG_C       = 0b00000001;
            const FLAG_Z       = 0b00000010;
            const FLAG_I       = 0b00000100;
            const FLAG_D       = 0b00001000;
            const FLAG_B       = 0b00010000;
            const FLAG_R       = 0b00100000;
            const FLAG_V       = 0b01000000;
            const FLAG_N       = 0b10000000;
            const FLAG_ALL     = Self::FLAG_C.bits()
                               | Self::FLAG_Z.bits()
                               | Self::FLAG_I.bits()
                               | Self::FLAG_D.bits()
                               | Self::FLAG_B.bits()
                               | Self::FLAG_R.bits()
                               | Self::FLAG_V.bits()
                               | Self::FLAG_N.bits();
        }
    }

    impl fmt::Display for ProcessorStatusFlags {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{:032b}", self.bits())
        }
    }

    pub struct Register {
        // 6502 register
        pub pc: u16,            //Program Counter
        pub sp: u8,             //Stack pointer
        pub p: ProcessorStatus, //Processor status
        pub a: u8,              //Accumulator
        pub x: u8,              //Index register X
        pub y: u8,              //Index register Y
    }

    impl Register {
        pub fn new() -> Self {
            Register {
                pc: 0,
                sp: 0xfd,
                p: { ProcessorStatus(0x34) },
                a: 0,
                x: 0,
                y: 0,
            }
        }

        pub fn reset(&mut self, pcl: u8, pch: u8) {
            *self = Register {
                pc: (pch as u16) << 8 | (pcl as u16),
                sp: 0xfd,
                p: { ProcessorStatus(0x34) },
                a: 0,
                x: 0,
                y: 0,
            };
        }
    }

    pub struct Core {
        pub reg: Register,
        pub irq_inhibit: bool,
        pub irq_counter: u8,
        pub irq_flag: u8,
        pub irq_in_process: u8,
        pub bus_to_cpu_mem: *mut dyn Bus,
    }

    impl Irq for Core {
        fn nmi_handler(&mut self) {
            let mut val_pc = self.reg.pc;
            self.mem_push((val_pc >> 8) as u8);
            self.mem_push((val_pc & 0xff) as u8);
            self.mem_push((self.reg.p.0 | ProcessorStatusFlags::FLAG_R.bits()) as u8);
            self.reg.p.set_i(true);
            self.irq_inhibit = true;
            val_pc = self.mem_read(NMI_VECT_ADDR) as u16;
            val_pc |= (self.mem_read(NMI_VECT_ADDR + 1) as u16) << 8;
            self.reg.pc = val_pc;
            cpu_cycles_add(7);
        }

        fn irq_handler(&mut self) {
            let mut val_pc = self.reg.pc;
            self.mem_push((val_pc >> 8) as u8);
            self.mem_push((val_pc & 0xff) as u8);
            self.mem_push((self.reg.p.0 | ProcessorStatusFlags::FLAG_R.bits()) as u8);
            self.reg.p.set_i(true);
            self.irq_inhibit = true;
            val_pc = self.mem_read(IRQ_VECT_ADDR) as u16;
            val_pc |= (self.mem_read(IRQ_VECT_ADDR + 1) as u16) << 8;
            self.reg.pc = val_pc;
            cpu_cycles_add(7);
        }

        fn irq_req(&mut self) {
            if self.irq_inhibit {
                self.irq_flag = 1;
            } else {
                self.irq_counter = 1;
            }
        }

        fn irq_ack(&mut self) {
            self.irq_flag = 0;
            self.irq_counter = 0;
        }
    }

    impl Core {
        pub fn new<'a>(mem: *mut dyn Bus) -> Core {
            Core {
                reg: Register::new(),
                irq_inhibit: false,
                irq_counter: 0,
                irq_flag: 0,
                irq_in_process: 0,
                bus_to_cpu_mem: mem,
            }
        }
        fn mem_read(&mut self, addr: u16) -> u8 {
            unsafe { (*self.bus_to_cpu_mem).read(addr) }
        }
        fn mem_push(&mut self, data: u8) {
            unsafe {
                (*self.bus_to_cpu_mem).push(&mut self.reg.sp, data);
            }
        }

        pub fn reset(&mut self) {
            let pcl = self.mem_read(REST_VECT_ADDR);
            let pch = self.mem_read(REST_VECT_ADDR + 1);

            self.reg.reset(pcl, pch);
            self.irq_inhibit = false;
            self.irq_counter = 0;
            self.irq_flag = 0;
            self.irq_in_process = 0;
        }

        pub fn execute_one_instruction(&mut self, mem: &mut MemMap) {
            let machine_code: u8;
            let cpu_reg: &mut Register = &mut self.reg;

            if self.irq_counter != 0 {
                self.irq_counter -= 1;
                if self.irq_counter == 0 {
                    self.irq_in_process = 1;
                    self.irq_handler();
                    return;
                }
            }
            machine_code = mem.read(cpu_reg.pc);
            // print!(
            //     "code addr {:#x}, machine_code {:#x}, ",
            //     cpu_reg.pc, machine_code
            // );
            cpu_reg.pc = cpu_reg.pc.wrapping_add(1);

            let instru_addring = &ISTRU_OP_CODE[machine_code as usize];
            cpu_cycles_add(instru_addring.instru_base_cycle as u16);

            let op_addr = instru_addring.addring_mode.addressing(self, mem);
            // print!(
            //     "asm instruction {}, operation addr {:#x}, ",
            //     instru_addring.instru_name, op_addr
            // );

            instru_addring.instru.exec(self, mem, op_addr);

            // println!(
            //     "a {:#x}, x {:#x}, y {:#x}, p {:#x}, sp {:#x}, cycles {}",
            //     cpu_reg.a, cpu_reg.x, cpu_reg.y, cpu_reg.p.0, cpu_reg.sp, get_cpu_cycles()
            // );
            // println!("{:#?}", instru_addring);
        }

        pub fn execute_instruction_until(&mut self, mem: &mut MemMap, cpu_cycles_end: u16) {
            while get_cpu_cycles() <= cpu_cycles_end {
                self.execute_one_instruction(mem);
            }
        }

        pub fn execute_instruction_until_and_hook<F>(
            &mut self,
            mem: &mut MemMap,
            cpu_cycles_end: u16,
            mut hook: F,
        ) where
            F: FnMut(&mut Register, &mut MemMap, u16),
        {
            let mut t = get_cpu_cycles();
            let mut n;
            while t <= cpu_cycles_end {
                self.execute_one_instruction(mem);
                n = get_cpu_cycles();
                hook(&mut self.reg, mem, n.saturating_sub(t));
                t = n;
            }
        }
    }

    #[derive(Debug)]
    enum AddressingMode {
        Acc,     //操作累加器 A: Op Accumulator
        Imp,     //隐含寻址 Implied Addressing
        Imm,     //立即寻址 Immediate Addressing
        Abs,     //绝对寻址 Absolute Addressing 又称直接寻址
        ZpAbs,   //零页寻址 全称绝对零页寻址 Zero-page Absolute Addressing
        AbsXI,   //绝对 X 变址 Absolute X Indexed Addressing
        AbsXI1C, //绝对 X 变址，跨页读写多 1 个周期
        AbsYI,   //绝对 Y 变址 Absolute Y Indexed Addressing
        AbsYI1C, //绝对 Y 变址，跨页读写多 1 个周期
        ZpXI,    //零页 X 变址 Zero-page X Indexed Addressing
        ZpYI,    //零页 Y 变址 Zero-page Y Indexed Addressing
        Ind,     //间接寻址 Indirect Addressing
        IndX,    //间接 X 变址 (先变址 X 后间接寻址): Pre-indexed Indirect Addressing
        IndY,    //间接 Y 变址 (后变址 Y 间接寻址): Post-indexed Indirect Addressing
        IndY1C,  //间接 Y 变址 (后变址 Y 间接寻址)，跨页读写多 1 个周期
        Rel,     //相对寻址：Relative Addressing
        Unk,     //unknown
    }

    impl AddressingMode {
        pub fn addressing(&self, core: &mut Core, mem: &mut MemMap) -> u16 {
            let cpu_reg: &mut Register = &mut core.reg;
            match self {
                AddressingMode::Acc => 0,
                AddressingMode::Imp => 0,
                AddressingMode::Imm => {
                    let addr = cpu_reg.pc;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr
                }
                AddressingMode::Abs => {
                    let mut addr: u16 = mem.read(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr |= (mem.read(cpu_reg.pc) as u16) << 8;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr
                }
                AddressingMode::ZpAbs => {
                    let addr: u16 = mem.read(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr
                }
                AddressingMode::AbsXI => {
                    let mut addr: u16 = mem.read(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr |= (mem.read(cpu_reg.pc) as u16) << 8;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr.wrapping_add(cpu_reg.x as u16)
                }
                AddressingMode::AbsXI1C => {
                    let mut addr: u16 = mem.read(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr |= (mem.read(cpu_reg.pc) as u16) << 8;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    let val = addr.wrapping_add(cpu_reg.x as u16);
                    cpu_cycles_add((((addr ^ val) >> 8) & 1) as u16);
                    val
                }
                AddressingMode::AbsYI => {
                    let mut addr: u16 = mem.read(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr |= (mem.read(cpu_reg.pc) as u16) << 8;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr.wrapping_add(cpu_reg.y as u16)
                }
                AddressingMode::AbsYI1C => {
                    let mut addr: u16 = mem.read(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr |= (mem.read(cpu_reg.pc) as u16) << 8;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    let val = addr.wrapping_add(cpu_reg.y as u16);
                    cpu_cycles_add(((addr ^ val) >> 8) as u16 & 1);
                    val
                }
                AddressingMode::ZpXI => {
                    let addr: u16 = mem.read(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    (addr.wrapping_add(cpu_reg.x as u16)) & 0x00ff
                }
                AddressingMode::ZpYI => {
                    let addr: u16 = mem.read(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    (addr.wrapping_add(cpu_reg.y as u16)) & 0x00ff
                }
                AddressingMode::Ind => {
                    let mut addr1: u16 = mem.read(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr1 |= (mem.read(cpu_reg.pc) as u16) << 8;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    let addr2: u16 = (addr1 & 0xff00) | ((addr1.wrapping_add(1)) & 0x00ff); // 刻意实现 6502 的 BUG
                    (mem.read(addr1) as u16) | ((mem.read(addr2) as u16) << 8)
                }
                AddressingMode::IndX => {
                    let mut addr = mem.read(cpu_reg.pc);
                    addr = addr.wrapping_add(cpu_reg.x);
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    (mem.read(addr as u16) as u16)
                        | ((mem.read(addr.wrapping_add(1) as u16) as u16) << 8)
                }
                AddressingMode::IndY => {
                    let addr = mem.read(cpu_reg.pc);
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    let val = (mem.read(addr as u16) as u16)
                        | ((mem.read(addr.wrapping_add(1) as u16) as u16) << 8);
                    val.wrapping_add(cpu_reg.y as u16)
                }
                AddressingMode::IndY1C => {
                    let addr = mem.read(cpu_reg.pc);
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    let mut val = (mem.read(addr as u16) as u16)
                        | ((mem.read(addr.wrapping_add(1) as u16) as u16) << 8);
                    val = val.wrapping_add(cpu_reg.y as u16);
                    cpu_cycles_add((((addr as u16 ^ val) >> 8) & 1) as u16);
                    val
                }
                AddressingMode::Rel => {
                    let addr = mem.read(cpu_reg.pc) as i8;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    cpu_reg.pc.saturating_add_signed(addr as i16)
                }
                AddressingMode::Unk => unimplemented!("Unimplemented addressing mode!"),
            }
        }
    }

    #[derive(Debug)]
    enum Instruction {
        LDA,  // LDA--由存储器取数送入累加器 A    M -> A
        LDX,  // LDX--由存储器取数送入寄存器 X    M -> X
        LDY,  // LDY--由存储器取数送入寄存器 Y    M -> Y
        STA,  // STA--将累加器 A 的数送入存储器    A -> M
        STX,  // STX--将寄存器 X 的数送入存储器    X -> M
        STY,  // STY--将寄存器 Y 的数送入存储器    Y -> M
        TAX,  // 将累加器 A 的内容送入变址寄存器 X
        TXA,  // 将变址寄存器 X 的内容送入累加器 A
        TAY,  // 将累加器 A 的内容送入变址寄存器 Y
        TYA,  // 将变址寄存器 Y 的内容送入累加器 A
        TSX,  // 堆栈指针 S 的内容送入变址寄存器 X
        TXS,  // 变址寄存器 X 的内容送入堆栈指针 S
        ADC,  // ADC--累加器，存储器，进位标志 C 相加，结果送累加器 A  A+M+C -> A
        SBC,  // SBC--从累加器减去存储器和进位标志 C 取反，结果送累加器 A-M-(1-C) -> A
        INC,  // INC--存储器单元内容增 1  M+1 -> M
        DEC,  // DEC--存储器单元内容减 1  M-1 -> M
        INX,  // INX--X 寄存器 +1 X+1 -> X
        DEX,  // DEX--X 寄存器 -1 X-1 -> X
        INY,  // INY--Y 寄存器 +1 Y+1 -> Y
        DEY,  // DEY--Y 寄存器 -1 Y-1 -> Y
        AND,  // AND--存储器与累加器相与，结果送累加器  A∧M -> A
        ORA,  // ORA--存储器与累加器相或，结果送累加器  A∨M -> A
        EOR,  // EOR--存储器与累加器异或，结果送累加器  A≮M -> A
        CLC,  // CLC--清除进位标志 C         0 -> C
        SEC,  // SEC--设置进位标志 C         1 -> C
        CLD,  // CLD--清除十进标志 D         0 -> D
        SED,  // SED--设置十进标志 D         1 -> D
        CLV,  // CLV--清除溢出标志 V         0 -> V
        CLI,  // CLI--清除中断禁止 V         0 -> I
        SEI,  // SEI--设置中断禁止 V         1 -> I
        CMP,  // CMP--累加器和存储器比较
        CPX,  // CPX--寄存器 X 的内容和存储器比较
        CPY,  // CPY--寄存器 Y 的内容和存储器比较
        BIT,  // BIT--位测试
        ASL,  // ASL--算术左移 储存器
        ASLA, // ASL--算术左移 累加器
        LSR,  // LSR--算术右移 储存器
        LSRA, // LSR--算术右移 累加器
        ROL,  // ROL--循环算术左移 储存器
        ROLA, // ROL--循环算术左移 累加器
        ROR,  // ROR--循环算术右移 储存器
        RORA, // ROR--循环算术右移 累加器
        PHA,  // PHA--累加器进栈
        PLA,  // PLA--累加器出栈
        PHP,  // PHP--标志寄存器 P 进栈
        PLP,  // PLP--标志寄存器 P 出栈
        JMP,  // JMP--无条件跳转
        BEQ,  // 如果标志位 Z = 1 则转移，否则继续
        BNE,  // 如果标志位 Z = 0 则转移，否则继续
        BCS,  // 如果标志位 C = 1 则转移，否则继续
        BCC,  // 如果标志位 C = 0 则转移，否则继续
        BMI,  // 如果标志位 N = 1 则转移，否则继续
        BPL,  // 如果标志位 N = 0 则转移，否则继续
        BVS,  // 如果标志位 V = 1 则转移，否则继续
        BVC,  // 如果标志位 V = 0 则转移，否则继续
        JSR,  // 跳转到子程序
        RTS,  // 返回到主程序
        NOP,  // 无操作
        BRK,  // 强制中断
        RTI,  // 从中断返回
        //---  组合指令  ----------
        ALR, // [Unofficial&Combo] AND+LSR
        ASR, //= SFC_INS_ALR,// 有消息称是叫这个
        ANC, // [Unofficial&Combo] AND+N2C?
        AAC, //= SFC_INS_ANC,// 差不多一个意思
        ARR, // [Unofficial&Combo] AND+ROR [类似]
        AXS, // [Unofficial&Combo] AND+XSB?
        SBX, //= SFC_INS_AXS,// 一个意思
        LAX, // [Unofficial&Combo] LDA+TAX
        SAX, // [Unofficial&Combo] STA&STX [类似]
        //--- 读改写指令 ----------
        DCP, // [Unofficial& RMW ] DEC+CMP
        ISC, // [Unofficial& RMW ] INC+SBC
        ISB, //= SFC_INS_ISC,// 差不多一个意思
        RLA, // [Unofficial& RMW ] ROL+AND
        RRA, // [Unofficial& RMW ] ROR+AND
        SLO, // [Unofficial& RMW ] ASL+ORA
        SRE, // [Unofficial& RMW ] LSR+EOR

        LAS,
        XAA,
        AHX,
        TAS,
        SHX,
        SHY,
        HK2,
        UNK,
    }

    impl Instruction {
        pub fn exec(&self, core: &mut Core, mem: &mut MemMap, addr: u16) {
            let cpu_reg: &mut Register = &mut core.reg;
            match self {
                Instruction::LDA => {
                    // LDA--由存储器取数送入累加器 A    M -> A
                    cpu_reg.a = mem.read(addr);
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::LDX => {
                    // LDX--由存储器取数送入寄存器 X    M -> X
                    cpu_reg.x = mem.read(addr);
                    cpu_reg.p.check_set_n(cpu_reg.x);
                    cpu_reg.p.check_set_z(cpu_reg.x);
                }
                Instruction::LDY => {
                    // LDY--由存储器取数送入寄存器 Y    M -> Y
                    cpu_reg.y = mem.read(addr);
                    cpu_reg.p.check_set_n(cpu_reg.y);
                    cpu_reg.p.check_set_z(cpu_reg.y);
                }
                Instruction::STA => {
                    // STA--将累加器 A 的数送入存储器    A -> M
                    mem.write(addr, cpu_reg.a);
                }
                Instruction::STX => {
                    // STX--将寄存器 X 的数送入存储器    X -> M
                    mem.write(addr, cpu_reg.x);
                }
                Instruction::STY => {
                    // STY--将寄存器 Y 的数送入存储器    Y -> M
                    mem.write(addr, cpu_reg.y);
                }
                Instruction::TAX => {
                    // 将累加器 A 的内容送入变址寄存器 X
                    cpu_reg.x = cpu_reg.a;
                    cpu_reg.p.check_set_n(cpu_reg.x);
                    cpu_reg.p.check_set_z(cpu_reg.x);
                }
                Instruction::TXA => {
                    // 将变址寄存器 X 的内容送入累加器 A
                    cpu_reg.a = cpu_reg.x;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::TAY => {
                    // 将累加器 A 的内容送入变址寄存器 Y
                    cpu_reg.y = cpu_reg.a;
                    cpu_reg.p.check_set_n(cpu_reg.y);
                    cpu_reg.p.check_set_z(cpu_reg.y);
                }
                Instruction::TYA => {
                    // 将变址寄存器 Y 的内容送入累加器 A
                    cpu_reg.a = cpu_reg.y;
                    cpu_reg.p.check_set_n(cpu_reg.y);
                    cpu_reg.p.check_set_z(cpu_reg.y);
                }
                Instruction::TSX => {
                    // 堆栈指针 S 的内容送入变址寄存器 X
                    cpu_reg.x = cpu_reg.sp;
                    cpu_reg.p.check_set_n(cpu_reg.x);
                    cpu_reg.p.check_set_z(cpu_reg.x);
                }
                Instruction::TXS => {
                    // 变址寄存器 X 的内容送入堆栈指针 S
                    cpu_reg.sp = cpu_reg.x;
                }
                Instruction::ADC => {
                    // ADC--累加器，存储器，进位标志 C 相加，结果送累加器 A  A+M+C -> A
                    let src = mem.read(addr);
                    let reslt_u16: u16 = (cpu_reg.a as u16)
                        .wrapping_add(src as u16)
                        .wrapping_add(if cpu_reg.p.c() { 1u16 } else { 0u16 });
                    let reslt_u8: u8 = reslt_u16 as u8;

                    cpu_reg.p.check_set_c((reslt_u16 >> 8) as u8);
                    cpu_reg
                        .p
                        .check_set_v(!(cpu_reg.a ^ src) & (cpu_reg.a ^ reslt_u8) & 0x80);
                    cpu_reg.a = reslt_u8;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::SBC => {
                    // SBC--从累加器减去存储器和进位标志 C 取反，结果送累加器 A-M-(1-C) -> A
                    let src = mem.read(addr);
                    let reslt_u16: u16 = ((cpu_reg.a as i16)
                        - (src as i16)
                        - if cpu_reg.p.c() { 0i16 } else { 1i16 })
                        as u16;
                    let reslt_u8: u8 = reslt_u16 as u8;

                    cpu_reg
                        .p
                        .check_set_c(if reslt_u16 > 255 { 0u8 } else { 1u8 });
                    cpu_reg
                        .p
                        .check_set_v((cpu_reg.a ^ src) & (cpu_reg.a ^ reslt_u8) & 0x80);
                    cpu_reg.a = reslt_u8;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::INC => {
                    // INC--存储器单元内容增 1  M+1 -> M
                    let mut val = mem.read(addr);
                    val = val.wrapping_add(1);
                    mem.write(addr, val);
                    cpu_reg.p.check_set_n(val);
                    cpu_reg.p.check_set_z(val);
                }
                Instruction::DEC => {
                    // DEC--存储器单元内容减 1  M-1 -> M
                    let mut val = mem.read(addr);
                    val = val.wrapping_sub(1);
                    mem.write(addr, val);
                    cpu_reg.p.check_set_n(val);
                    cpu_reg.p.check_set_z(val);
                }
                Instruction::INX => {
                    // INX--X 寄存器 +1 X+1 -> X
                    cpu_reg.x = cpu_reg.x.wrapping_add(1);
                    cpu_reg.p.check_set_n(cpu_reg.x);
                    cpu_reg.p.check_set_z(cpu_reg.x);
                }
                Instruction::DEX => {
                    // DEX--X 寄存器 -1 X-1 -> X
                    cpu_reg.x = cpu_reg.x.wrapping_sub(1);
                    cpu_reg.p.check_set_n(cpu_reg.x);
                    cpu_reg.p.check_set_z(cpu_reg.x);
                }
                Instruction::INY => {
                    // INY--Y 寄存器 +1 Y+1 -> Y
                    cpu_reg.y = cpu_reg.y.wrapping_add(1);
                    cpu_reg.p.check_set_n(cpu_reg.y);
                    cpu_reg.p.check_set_z(cpu_reg.y);
                }
                Instruction::DEY => {
                    // DEY--Y 寄存器 -1 Y-1 -> Y
                    cpu_reg.y = cpu_reg.y.wrapping_sub(1);
                    cpu_reg.p.check_set_n(cpu_reg.y);
                    cpu_reg.p.check_set_z(cpu_reg.y);
                }
                Instruction::AND => {
                    // AND--存储器与累加器相与，结果送累加器  A&M -> A
                    cpu_reg.a &= mem.read(addr);
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::ORA => {
                    // ORA--存储器与累加器相或，结果送累加器  A|M -> A
                    cpu_reg.a |= mem.read(addr);
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::EOR => {
                    // EOR--存储器与累加器异或，结果送累加器  A≮M -> A
                    cpu_reg.a ^= mem.read(addr);
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::CLC => {
                    // CLC--清除进位标志 C         0 -> C
                    cpu_reg.p.set_c(false);
                }
                Instruction::SEC => {
                    // SEC--设置进位标志 C         1 -> C
                    cpu_reg.p.set_c(true);
                }
                Instruction::CLD => {
                    // CLD--清除十进标志 D         0 -> D
                    cpu_reg.p.set_d(false);
                }
                Instruction::SED => {
                    // SED--设置十进标志 D         1 -> D
                    cpu_reg.p.set_d(true);
                }
                Instruction::CLV => {
                    // CLV--清除溢出标志 V         0 -> V
                    cpu_reg.p.set_v(false);
                }
                Instruction::CLI => {
                    // CLI--清除中断禁止 V         0 -> I
                    cpu_reg.p.set_i(false);
                    core.irq_inhibit = false;
                    core.irq_counter = core.irq_flag << 1;
                }
                Instruction::SEI => {
                    // SEI--设置中断禁止 V         1 -> I
                    cpu_reg.p.set_i(true);
                    core.irq_inhibit = true;
                }
                Instruction::CMP => {
                    // CMP--累加器和存储器比较
                    let reslt: u16 = (cpu_reg.a as i16 - mem.read(addr) as i16) as u16;
                    cpu_reg.p.set_c(reslt & 0x8000 == 0);
                    cpu_reg.p.check_set_n(reslt as u8);
                    cpu_reg.p.check_set_z(reslt as u8);
                }
                Instruction::CPX => {
                    // CPX--寄存器 X 的内容和存储器比较
                    let reslt: u16 = (cpu_reg.x as i16 - mem.read(addr) as i16) as u16;
                    cpu_reg.p.set_c(reslt < 256);
                    cpu_reg.p.check_set_n(reslt as u8);
                    cpu_reg.p.check_set_z(reslt as u8);
                }
                Instruction::CPY => {
                    // CPY--寄存器 Y 的内容和存储器比较
                    let reslt: u16 = (cpu_reg.y as i16 - mem.read(addr) as i16) as u16;
                    cpu_reg.p.set_c(reslt < 256);
                    cpu_reg.p.check_set_n(reslt as u8);
                    cpu_reg.p.check_set_z(reslt as u8);
                }
                Instruction::BIT => {
                    // BIT--位测试
                    let val = mem.read(addr);
                    cpu_reg.p.check_set_v(val & 0x40);
                    cpu_reg.p.check_set_n(val);
                    cpu_reg.p.check_set_z(cpu_reg.a & val);
                }
                Instruction::ASL => {
                    // ASL--算术左移 储存器
                    let mut val = mem.read(addr);
                    cpu_reg.p.check_set_c(val & 0x80);
                    val = val << 1;
                    mem.write(addr, val);
                    cpu_reg.p.check_set_n(val);
                    cpu_reg.p.check_set_z(val);
                }
                Instruction::ASLA => {
                    // ASL--算术左移 累加器
                    cpu_reg.p.check_set_c(cpu_reg.a & 0x80);
                    cpu_reg.a = cpu_reg.a << 1;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::LSR => {
                    // LSR--算术右移 储存器
                    let mut val = mem.read(addr);
                    cpu_reg.p.check_set_c(val & 0x01);
                    val = val >> 1;
                    mem.write(addr, val);
                    cpu_reg.p.check_set_n(val);
                    cpu_reg.p.check_set_z(val);
                }
                Instruction::LSRA => {
                    // LSR--算术右移 累加器
                    cpu_reg.p.check_set_c(cpu_reg.a & 0x01);
                    cpu_reg.a = cpu_reg.a >> 1;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::ROL => {
                    // ROL--循环算术左移 储存器
                    let mut val_u16: u16 = mem.read(addr) as u16;
                    val_u16 <<= 1;
                    val_u16 |= if cpu_reg.p.c() { 1 } else { 0 };
                    cpu_reg.p.check_set_c((val_u16 >> 8) as u8);
                    let val_u8 = val_u16 as u8;
                    mem.write(addr, val_u8);
                    cpu_reg.p.check_set_n(val_u8);
                    cpu_reg.p.check_set_z(val_u8);
                }
                Instruction::ROLA => {
                    // ROL--循环算术左移 累加器
                    let mut val_u16: u16 = cpu_reg.a as u16;
                    val_u16 <<= 1;
                    val_u16 |= if cpu_reg.p.c() { 1 } else { 0 };
                    cpu_reg.p.check_set_c((val_u16 >> 8) as u8);
                    cpu_reg.a = val_u16 as u8;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::ROR => {
                    // ROR--循环算术右移 储存器
                    let mut val_u16: u16 = mem.read(addr) as u16;
                    val_u16 |= (if cpu_reg.p.c() { 1 } else { 0 }) << 8;
                    cpu_reg.p.check_set_c((val_u16 & 0x1) as u8);
                    let val_u8 = (val_u16 >> 1) as u8;
                    mem.write(addr, val_u8);
                    cpu_reg.p.check_set_n(val_u8);
                    cpu_reg.p.check_set_z(val_u8);
                }
                Instruction::RORA => {
                    // ROR--循环算术右移 累加器
                    let mut val_u16: u16 = cpu_reg.a as u16;
                    val_u16 |= (if cpu_reg.p.c() { 1 } else { 0 }) << 8;
                    cpu_reg.p.check_set_c((val_u16 & 0x1) as u8);
                    cpu_reg.a = (val_u16 >> 1) as u8;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::PHA => {
                    // PHA--累加器进栈
                    mem.push(&mut cpu_reg.sp, cpu_reg.a);
                }
                Instruction::PLA => {
                    // PLA--累加器出栈
                    cpu_reg.a = mem.pop(&mut cpu_reg.sp);
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::PHP => {
                    // PHP--标志寄存器 P 进栈
                    mem.push(
                        &mut cpu_reg.sp,
                        cpu_reg.p.0
                            | ProcessorStatusFlags::FLAG_B.bits()
                            | ProcessorStatusFlags::FLAG_R.bits(),
                    );
                }
                Instruction::PLP => {
                    // PLP--标志寄存器 P 出栈
                    cpu_reg.p.0 = mem.pop(&mut cpu_reg.sp);
                    cpu_reg.p.set_b(false);
                    cpu_reg.p.set_r(true);
                    if cpu_reg.p.i() == false {
                        core.irq_counter = core.irq_flag << 1;
                    }
                }
                Instruction::JMP => {
                    // JMP--无条件跳转
                    cpu_reg.pc = addr;
                }
                Instruction::BEQ => {
                    // 如果标志位 Z = 1 则转移，否则继续
                    if cpu_reg.p.z() {
                        self.brunch_jmp(cpu_reg, addr);
                    }
                }
                Instruction::BNE => {
                    // 如果标志位 Z = 0 则转移，否则继续
                    if cpu_reg.p.z() == false {
                        self.brunch_jmp(cpu_reg, addr);
                    }
                }
                Instruction::BCS => {
                    // 如果标志位 C = 1 则转移，否则继续
                    if cpu_reg.p.c() {
                        self.brunch_jmp(cpu_reg, addr);
                    }
                }
                Instruction::BCC => {
                    // 如果标志位 C = 0 则转移，否则继续
                    if cpu_reg.p.c() == false {
                        self.brunch_jmp(cpu_reg, addr);
                    }
                }
                Instruction::BMI => {
                    // 如果标志位 N = 1 则转移，否则继续
                    if cpu_reg.p.n() {
                        self.brunch_jmp(cpu_reg, addr);
                    }
                }
                Instruction::BPL => {
                    // 如果标志位 N = 0 则转移，否则继续
                    if cpu_reg.p.n() == false {
                        self.brunch_jmp(cpu_reg, addr);
                    }
                }
                Instruction::BVS => {
                    if cpu_reg.p.v() {
                        self.brunch_jmp(cpu_reg, addr);
                    }
                } // 如果标志位 V = 1 则转移，否则继续
                Instruction::BVC => {
                    // 如果标志位 V = 0 则转移，否则继续
                    if cpu_reg.p.v() == false {
                        self.brunch_jmp(cpu_reg, addr);
                    }
                }
                Instruction::JSR => {
                    // 跳转到子程序
                    let val = cpu_reg.pc.wrapping_sub(1);
                    mem.push(&mut cpu_reg.sp, (val >> 8) as u8);
                    mem.push(&mut cpu_reg.sp, val as u8);
                    cpu_reg.pc = addr;
                }
                Instruction::RTS => {
                    // 返回到主程序
                    let pcl: u16 = mem.pop(&mut cpu_reg.sp) as u16;
                    let pch: u16 = mem.pop(&mut cpu_reg.sp) as u16;
                    cpu_reg.pc = pcl | (pch << 8);
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                }
                Instruction::NOP => {} // 无操作
                Instruction::BRK => {
                    // 强制中断
                    let val = cpu_reg.pc.wrapping_add(1);
                    mem.push(&mut cpu_reg.sp, (val >> 8) as u8);
                    mem.push(&mut cpu_reg.sp, val as u8);
                    mem.push(
                        &mut cpu_reg.sp,
                        cpu_reg.p.0
                            | ProcessorStatusFlags::FLAG_B.bits()
                            | ProcessorStatusFlags::FLAG_R.bits(),
                    );
                    cpu_reg.p.set_i(true);
                    core.irq_inhibit = true;
                    let pcl: u16 = mem.read(BRK_VECT_ADDR) as u16;
                    let pch: u16 = mem.read(BRK_VECT_ADDR + 1) as u16;
                    cpu_reg.pc = pcl | (pch << 8);
                }
                Instruction::RTI => {
                    // 从中断返回

                    // ps 出栈
                    cpu_reg.p.0 = mem.pop(&mut cpu_reg.sp);
                    cpu_reg.p.set_b(false);
                    cpu_reg.p.set_r(true);
                    // pc 出栈
                    let pcl: u16 = mem.pop(&mut cpu_reg.sp) as u16;
                    let pch: u16 = mem.pop(&mut cpu_reg.sp) as u16;
                    cpu_reg.pc = pcl | (pch << 8);
                    // 清除计数
                    core.irq_counter =
                        core.irq_in_process & core.irq_flag & ((!cpu_reg.p.i()) as u8);
                    core.irq_in_process = 0;
                }
                //---  组合指令  ----------
                Instruction::ASR | Instruction::ALR => {
                    // [Unofficial&Combo] AND+LSR
                    //= SFC_INS_ALR{}
                    cpu_reg.a &= mem.read(addr);
                    cpu_reg.p.check_set_c(cpu_reg.a & 0x1);
                    cpu_reg.a >>= 1;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::ANC | Instruction::AAC => {
                    // [Unofficial&Combo] AND+N2C?
                    cpu_reg.a &= mem.read(addr);
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                    cpu_reg.p.set_c(cpu_reg.p.n());
                }
                Instruction::ARR => {
                    // [Unofficial&Combo] AND+ROR [类似]
                    cpu_reg.a &= mem.read(addr);
                    cpu_reg.a = (cpu_reg.a >> 1) | if cpu_reg.p.c() { 1 << 7 } else { 0 };
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                    cpu_reg.p.check_set_c((cpu_reg.a >> 6) & 0x1);
                    cpu_reg
                        .p
                        .check_set_v(((cpu_reg.a >> 5) ^ (cpu_reg.a >> 6)) & 0x1);
                }
                Instruction::AXS | Instruction::SBX => {
                    // [Unofficial&Combo] AND+XSB?
                    let val = (cpu_reg.a & cpu_reg.x) as i16 - mem.read(addr) as i16;
                    cpu_reg.x = (val & 0xff) as u8;
                    cpu_reg.p.check_set_n(cpu_reg.x);
                    cpu_reg.p.check_set_z(cpu_reg.x);
                    cpu_reg.p.set_c(val >= 0);
                }
                Instruction::LAX => {
                    // [Unofficial&Combo] LDA+TAX
                    cpu_reg.a = mem.read(addr);
                    cpu_reg.x = cpu_reg.a;
                    cpu_reg.p.check_set_n(cpu_reg.x);
                    cpu_reg.p.check_set_z(cpu_reg.x);
                }
                Instruction::SAX => {
                    // [Unofficial&Combo] STA&STX [类似]
                    mem.write(addr, cpu_reg.a & cpu_reg.x);
                }
                //--- 读改写指令 ----------
                Instruction::DCP => {
                    // [Unofficial& RMW ] DEC+CMP
                    let mut val = mem.read(addr);
                    val = val.wrapping_sub(1);
                    mem.write(addr, val);

                    let val_i16 = cpu_reg.a as i16 - val as i16;
                    cpu_reg.p.set_c(val_i16 >= 0);
                    cpu_reg.p.check_set_n((val_i16 & 0xff) as u8);
                    cpu_reg.p.check_set_z((val_i16 & 0xff) as u8);
                }
                Instruction::ISC | Instruction::ISB => {
                    // [Unofficial& RMW ] INC+SBC
                    //INC
                    let mut val = mem.read(addr);
                    val = val.wrapping_add(1);
                    mem.write(addr, val);
                    //SBC
                    let src = val;
                    let reslt_u16: u16 = ((cpu_reg.a as i16)
                        - (src as i16)
                        - if cpu_reg.p.c() { 0i16 } else { 1i16 })
                        as u16;
                    let reslt_u8: u8 = reslt_u16 as u8;

                    cpu_reg
                        .p
                        .check_set_c(if reslt_u16 > 255 { 0u8 } else { 1u8 });
                    cpu_reg
                        .p
                        .check_set_v((cpu_reg.a ^ src) & (cpu_reg.a ^ reslt_u8) & 0x80);
                    cpu_reg.a = reslt_u8;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::RLA => {
                    // [Unofficial& RMW ] ROL+AND
                    // ROL
                    let mut val_u16: u16 = mem.read(addr) as u16;
                    val_u16 <<= 1;
                    if cpu_reg.p.c() {
                        val_u16 |= 1;
                    }
                    cpu_reg.p.set_c(val_u16 > 0xff);
                    let val_u8: u8 = val_u16 as u8;
                    mem.write(addr, val_u8);
                    // AND
                    cpu_reg.a &= val_u8;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::RRA => {
                    // [Unofficial& RMW ] ROR+AND
                    let mut val_u16: u16 = mem.read(addr) as u16;

                    if cpu_reg.p.c() {
                        val_u16 |= 0x100;
                    }
                    cpu_reg.p.check_set_c((val_u16 & 0x1) as u8);
                    val_u16 >>= 1;

                    //ADC
                    let src = val_u16 as u8;
                    mem.write(addr, src);
                    let reslt_u16: u16 = (cpu_reg.a as u16)
                        .wrapping_add(src as u16)
                        .wrapping_add(if cpu_reg.p.c() { 1u16 } else { 0u16 });
                    let reslt_u8: u8 = reslt_u16 as u8;

                    cpu_reg.p.check_set_c((reslt_u16 >> 8) as u8);
                    cpu_reg
                        .p
                        .check_set_v(!(cpu_reg.a ^ src) & (cpu_reg.a ^ reslt_u8) & 0x80);
                    cpu_reg.a = reslt_u8;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::SLO => {
                    // [Unofficial& RMW ] ASL+ORA
                    // ASL
                    let mut val = mem.read(addr);
                    cpu_reg.p.check_set_c(val & 0x80);
                    val = val << 1;
                    mem.write(addr, val);
                    // ORA
                    cpu_reg.a |= val;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::SRE => {
                    // LSR
                    let mut val = mem.read(addr);
                    cpu_reg.p.check_set_c(val & 0x1);
                    val = val >> 1;
                    mem.write(addr, val);
                    // EOR
                    cpu_reg.a ^= val;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                } // [Unofficial& RMW ] LSR+EOR

                Instruction::HK2 => {
                    // HK2: Hack $02 - 用于提示 NSF 初始化
                }
                Instruction::LAS
                | Instruction::XAA
                | Instruction::AHX
                | Instruction::TAS
                | Instruction::SHX
                | Instruction::SHY
                | Instruction::UNK => {
                    unimplemented!("Undefine instruction!");
                }
            }
        }

        fn brunch_jmp(&self, cpu_reg: &mut Register, addr: u16) {
            cpu_cycles_add((((addr ^ cpu_reg.pc) >> 8) & 1) as u16 + 1);
            cpu_reg.pc = addr;
        }
    }

    #[derive(Debug)]
    struct InstruAddring<'a> {
        instru_name: &'a str,
        instru: Instruction,
        instru_base_cycle: u8,
        addring_mode: AddressingMode,
    }

    #[rustfmt::skip]
    const ISTRU_OP_CODE: [InstruAddring; 256] = [
        InstruAddring { instru_name: "BRK", instru: Instruction::BRK,  instru_base_cycle : 7, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "ORA", instru: Instruction::ORA,  instru_base_cycle : 6, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "HK2", instru: Instruction::HK2,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "SLO", instru: Instruction::SLO,  instru_base_cycle : 8, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "ORA", instru: Instruction::ORA,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "ASL", instru: Instruction::ASL,  instru_base_cycle : 5, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "SLO", instru: Instruction::SLO,  instru_base_cycle : 5, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "PHP", instru: Instruction::PHP,  instru_base_cycle : 3, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "ORA", instru: Instruction::ORA,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "ASLA", instru: Instruction::ASLA,instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "ANC", instru: Instruction::ANC,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "ORA", instru: Instruction::ORA,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "ASL", instru: Instruction::ASL,  instru_base_cycle : 6, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "SLO", instru: Instruction::SLO,  instru_base_cycle : 6, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "BPL", instru: Instruction::BPL,  instru_base_cycle : 2, addring_mode: AddressingMode::Rel },
        InstruAddring { instru_name: "ORA", instru: Instruction::ORA,  instru_base_cycle : 5, addring_mode: AddressingMode::IndY1C },
        InstruAddring { instru_name: "UNK", instru: Instruction::UNK,  instru_base_cycle : 2, addring_mode: AddressingMode::Unk },
        InstruAddring { instru_name: "SLO", instru: Instruction::SLO,  instru_base_cycle : 8, addring_mode: AddressingMode::IndY },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "ORA", instru: Instruction::ORA,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "ASL", instru: Instruction::ASL,  instru_base_cycle : 6, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "SLO", instru: Instruction::SLO,  instru_base_cycle : 6, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "CLC", instru: Instruction::CLC,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "ORA", instru: Instruction::ORA,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsYI1C },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "SLO", instru: Instruction::SLO,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsYI },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsXI1C },
        InstruAddring { instru_name: "ORA", instru: Instruction::ORA,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsXI1C },
        InstruAddring { instru_name: "ASL", instru: Instruction::ASL,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsXI },
        InstruAddring { instru_name: "SLO", instru: Instruction::SLO,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsXI },
        InstruAddring { instru_name: "JSR", instru: Instruction::JSR,  instru_base_cycle : 6, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "AND", instru: Instruction::AND,  instru_base_cycle : 6, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "UNK", instru: Instruction::UNK,  instru_base_cycle : 2, addring_mode: AddressingMode::Unk },
        InstruAddring { instru_name: "RLA", instru: Instruction::RLA,  instru_base_cycle : 8, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "BIT", instru: Instruction::BIT,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "AND", instru: Instruction::AND,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "ROL", instru: Instruction::ROL,  instru_base_cycle : 5, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "RLA", instru: Instruction::RLA,  instru_base_cycle : 5, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "PLP", instru: Instruction::PLP,  instru_base_cycle : 4, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "AND", instru: Instruction::AND,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "ROLA", instru: Instruction::ROLA,instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "ANC", instru: Instruction::ANC,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "BIT", instru: Instruction::BIT,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "AND", instru: Instruction::AND,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "ROL", instru: Instruction::ROL,  instru_base_cycle : 6, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "RLA", instru: Instruction::RLA,  instru_base_cycle : 6, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "BMI", instru: Instruction::BMI,  instru_base_cycle : 2, addring_mode: AddressingMode::Rel },
        InstruAddring { instru_name: "AND", instru: Instruction::AND,  instru_base_cycle : 5, addring_mode: AddressingMode::IndY1C },
        InstruAddring { instru_name: "UNK", instru: Instruction::UNK,  instru_base_cycle : 2, addring_mode: AddressingMode::Unk },
        InstruAddring { instru_name: "RLA", instru: Instruction::RLA,  instru_base_cycle : 8, addring_mode: AddressingMode::IndY },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "AND", instru: Instruction::AND,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "ROL", instru: Instruction::ROL,  instru_base_cycle : 6, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "RLA", instru: Instruction::RLA,  instru_base_cycle : 6, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "SEC", instru: Instruction::SEC,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "AND", instru: Instruction::AND,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsYI1C },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "RLA", instru: Instruction::RLA,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsYI },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsXI1C },
        InstruAddring { instru_name: "AND", instru: Instruction::AND,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsXI1C },
        InstruAddring { instru_name: "ROL", instru: Instruction::ROL,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsXI },
        InstruAddring { instru_name: "RLA", instru: Instruction::RLA,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsXI },
        InstruAddring { instru_name: "RTI", instru: Instruction::RTI,  instru_base_cycle : 6, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "EOR", instru: Instruction::EOR,  instru_base_cycle : 6, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "UNK", instru: Instruction::UNK,  instru_base_cycle : 2, addring_mode: AddressingMode::Unk },
        InstruAddring { instru_name: "SRE", instru: Instruction::SRE,  instru_base_cycle : 8, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "EOR", instru: Instruction::EOR,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "LSR", instru: Instruction::LSR,  instru_base_cycle : 5, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "SRE", instru: Instruction::SRE,  instru_base_cycle : 5, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "PHA", instru: Instruction::PHA,  instru_base_cycle : 3, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "EOR", instru: Instruction::EOR,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "LSRA", instru: Instruction::LSRA,instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "ASR", instru: Instruction::ASR,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "JMP", instru: Instruction::JMP,  instru_base_cycle : 3, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "EOR", instru: Instruction::EOR,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "LSR", instru: Instruction::LSR,  instru_base_cycle : 6, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "SRE", instru: Instruction::SRE,  instru_base_cycle : 6, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "BVC", instru: Instruction::BVC,  instru_base_cycle : 2, addring_mode: AddressingMode::Rel },
        InstruAddring { instru_name: "EOR", instru: Instruction::EOR,  instru_base_cycle : 5, addring_mode: AddressingMode::IndY1C },
        InstruAddring { instru_name: "UNK", instru: Instruction::UNK,  instru_base_cycle : 2, addring_mode: AddressingMode::Unk },
        InstruAddring { instru_name: "SRE", instru: Instruction::SRE,  instru_base_cycle : 8, addring_mode: AddressingMode::IndY },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "EOR", instru: Instruction::EOR,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "LSR", instru: Instruction::LSR,  instru_base_cycle : 6, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "SRE", instru: Instruction::SRE,  instru_base_cycle : 6, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "CLI", instru: Instruction::CLI,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "EOR", instru: Instruction::EOR,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsYI1C },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "SRE", instru: Instruction::SRE,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsYI },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsXI1C },
        InstruAddring { instru_name: "EOR", instru: Instruction::EOR,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsXI1C },
        InstruAddring { instru_name: "LSR", instru: Instruction::LSR,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsXI },
        InstruAddring { instru_name: "SRE", instru: Instruction::SRE,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsXI },
        InstruAddring { instru_name: "RTS", instru: Instruction::RTS,  instru_base_cycle : 6, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "ADC", instru: Instruction::ADC,  instru_base_cycle : 6, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "UNK", instru: Instruction::UNK,  instru_base_cycle : 2, addring_mode: AddressingMode::Unk },
        InstruAddring { instru_name: "RRA", instru: Instruction::RRA,  instru_base_cycle : 8, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "ADC", instru: Instruction::ADC,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "ROR", instru: Instruction::ROR,  instru_base_cycle : 5, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "RRA", instru: Instruction::RRA,  instru_base_cycle : 5, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "PLA", instru: Instruction::PLA,  instru_base_cycle : 4, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "ADC", instru: Instruction::ADC,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "RORA", instru: Instruction::RORA,instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "ARR", instru: Instruction::ARR,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "JMP", instru: Instruction::JMP,  instru_base_cycle : 5, addring_mode: AddressingMode::Ind },
        InstruAddring { instru_name: "ADC", instru: Instruction::ADC,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "ROR", instru: Instruction::ROR,  instru_base_cycle : 6, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "RRA", instru: Instruction::RRA,  instru_base_cycle : 6, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "BVS", instru: Instruction::BVS,  instru_base_cycle : 2, addring_mode: AddressingMode::Rel },
        InstruAddring { instru_name: "ADC", instru: Instruction::ADC,  instru_base_cycle : 5, addring_mode: AddressingMode::IndY1C },
        InstruAddring { instru_name: "UNK", instru: Instruction::UNK,  instru_base_cycle : 2, addring_mode: AddressingMode::Unk },
        InstruAddring { instru_name: "RRA", instru: Instruction::RRA,  instru_base_cycle : 8, addring_mode: AddressingMode::IndY },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "ADC", instru: Instruction::ADC,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "ROR", instru: Instruction::ROR,  instru_base_cycle : 6, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "RRA", instru: Instruction::RRA,  instru_base_cycle : 6, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "SEI", instru: Instruction::SEI,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "ADC", instru: Instruction::ADC,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsYI1C },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "RRA", instru: Instruction::RRA,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsYI },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsXI1C },
        InstruAddring { instru_name: "ADC", instru: Instruction::ADC,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsXI1C },
        InstruAddring { instru_name: "ROR", instru: Instruction::ROR,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsXI },
        InstruAddring { instru_name: "RRA", instru: Instruction::RRA,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsXI },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "STA", instru: Instruction::STA,  instru_base_cycle : 6, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "SAX", instru: Instruction::SAX,  instru_base_cycle : 6, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "STY", instru: Instruction::STY,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "STA", instru: Instruction::STA,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "STX", instru: Instruction::STX,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "SAX", instru: Instruction::SAX,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "DEY", instru: Instruction::DEY,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "TXA", instru: Instruction::TXA,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "XAA", instru: Instruction::XAA,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "STY", instru: Instruction::STY,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "STA", instru: Instruction::STA,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "STX", instru: Instruction::STX,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "SAX", instru: Instruction::SAX,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "BCC", instru: Instruction::BCC,  instru_base_cycle : 2, addring_mode: AddressingMode::Rel },
        InstruAddring { instru_name: "STA", instru: Instruction::STA,  instru_base_cycle : 6, addring_mode: AddressingMode::IndY },
        InstruAddring { instru_name: "UNK", instru: Instruction::UNK,  instru_base_cycle : 2, addring_mode: AddressingMode::Unk },
        InstruAddring { instru_name: "AHX", instru: Instruction::AHX,  instru_base_cycle : 6, addring_mode: AddressingMode::IndY1C },
        InstruAddring { instru_name: "STY", instru: Instruction::STY,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "STA", instru: Instruction::STA,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "STX", instru: Instruction::STX,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpYI },
        InstruAddring { instru_name: "SAX", instru: Instruction::SAX,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpYI },
        InstruAddring { instru_name: "TYA", instru: Instruction::TYA,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "STA", instru: Instruction::STA,  instru_base_cycle : 5, addring_mode: AddressingMode::AbsYI },
        InstruAddring { instru_name: "TXS", instru: Instruction::TXS,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "TAS", instru: Instruction::TAS,  instru_base_cycle : 5, addring_mode: AddressingMode::AbsYI1C },
        InstruAddring { instru_name: "SHY", instru: Instruction::SHY,  instru_base_cycle : 5, addring_mode: AddressingMode::AbsXI1C },
        InstruAddring { instru_name: "STA", instru: Instruction::STA,  instru_base_cycle : 5, addring_mode: AddressingMode::AbsXI },
        InstruAddring { instru_name: "SHX", instru: Instruction::SHX,  instru_base_cycle : 5, addring_mode: AddressingMode::AbsYI1C },
        InstruAddring { instru_name: "AHX", instru: Instruction::AHX,  instru_base_cycle : 5, addring_mode: AddressingMode::AbsYI1C },
        InstruAddring { instru_name: "LDY", instru: Instruction::LDY,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "LDA", instru: Instruction::LDA,  instru_base_cycle : 6, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "LDX", instru: Instruction::LDX,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "LAX", instru: Instruction::LAX,  instru_base_cycle : 6, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "LDY", instru: Instruction::LDY,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "LDA", instru: Instruction::LDA,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "LDX", instru: Instruction::LDX,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "LAX", instru: Instruction::LAX,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "TAY", instru: Instruction::TAY,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "LDA", instru: Instruction::LDA,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "TAX", instru: Instruction::TAX,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "LAX", instru: Instruction::LAX,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "LDY", instru: Instruction::LDY,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "LDA", instru: Instruction::LDA,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "LDX", instru: Instruction::LDX,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "LAX", instru: Instruction::LAX,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "BCS", instru: Instruction::BCS,  instru_base_cycle : 2, addring_mode: AddressingMode::Rel },
        InstruAddring { instru_name: "LDA", instru: Instruction::LDA,  instru_base_cycle : 5, addring_mode: AddressingMode::IndY1C },
        InstruAddring { instru_name: "UNK", instru: Instruction::UNK,  instru_base_cycle : 2, addring_mode: AddressingMode::Unk },
        InstruAddring { instru_name: "LAX", instru: Instruction::LAX,  instru_base_cycle : 5, addring_mode: AddressingMode::IndY1C },
        InstruAddring { instru_name: "LDY", instru: Instruction::LDY,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "LDA", instru: Instruction::LDA,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "LDX", instru: Instruction::LDX,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpYI },
        InstruAddring { instru_name: "LAX", instru: Instruction::LAX,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpYI },
        InstruAddring { instru_name: "CLV", instru: Instruction::CLV,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "LDA", instru: Instruction::LDA,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsYI1C },
        InstruAddring { instru_name: "TSX", instru: Instruction::TSX,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "LAS", instru: Instruction::LAS,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsYI1C },
        InstruAddring { instru_name: "LDY", instru: Instruction::LDY,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsXI1C },
        InstruAddring { instru_name: "LDA", instru: Instruction::LDA,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsXI1C },
        InstruAddring { instru_name: "LDX", instru: Instruction::LDX,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsYI1C },
        InstruAddring { instru_name: "LAX", instru: Instruction::LAX,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsYI1C },
        InstruAddring { instru_name: "CPY", instru: Instruction::CPY,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "CMP", instru: Instruction::CMP,  instru_base_cycle : 6, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "DCP", instru: Instruction::DCP,  instru_base_cycle : 8, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "CPY", instru: Instruction::CPY,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "CMP", instru: Instruction::CMP,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "DEC", instru: Instruction::DEC,  instru_base_cycle : 5, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "DCP", instru: Instruction::DCP,  instru_base_cycle : 5, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "INY", instru: Instruction::INY,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "CMP", instru: Instruction::CMP,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "DEX", instru: Instruction::DEX,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "AXS", instru: Instruction::AXS,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "CPY", instru: Instruction::CPY,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "CMP", instru: Instruction::CMP,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "DEC", instru: Instruction::DEC,  instru_base_cycle : 6, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "DCP", instru: Instruction::DCP,  instru_base_cycle : 6, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "BNE", instru: Instruction::BNE,  instru_base_cycle : 2, addring_mode: AddressingMode::Rel },
        InstruAddring { instru_name: "CMP", instru: Instruction::CMP,  instru_base_cycle : 5, addring_mode: AddressingMode::IndY1C },
        InstruAddring { instru_name: "UNK", instru: Instruction::UNK,  instru_base_cycle : 2, addring_mode: AddressingMode::Unk },
        InstruAddring { instru_name: "DCP", instru: Instruction::DCP,  instru_base_cycle : 8, addring_mode: AddressingMode::IndY },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "CMP", instru: Instruction::CMP,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "DEC", instru: Instruction::DEC,  instru_base_cycle : 6, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "DCP", instru: Instruction::DCP,  instru_base_cycle : 6, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "CLD", instru: Instruction::CLD,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "CMP", instru: Instruction::CMP,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsYI1C },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "DCP", instru: Instruction::DCP,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsYI },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsXI1C },
        InstruAddring { instru_name: "CMP", instru: Instruction::CMP,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsXI1C },
        InstruAddring { instru_name: "DEC", instru: Instruction::DEC,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsXI },
        InstruAddring { instru_name: "DCP", instru: Instruction::DCP,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsXI },
        InstruAddring { instru_name: "CPX", instru: Instruction::CPX,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "SBC", instru: Instruction::SBC,  instru_base_cycle : 6, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "ISB", instru: Instruction::ISB,  instru_base_cycle : 8, addring_mode: AddressingMode::IndX },
        InstruAddring { instru_name: "CPX", instru: Instruction::CPX,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "SBC", instru: Instruction::SBC,  instru_base_cycle : 3, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "INC", instru: Instruction::INC,  instru_base_cycle : 5, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "ISB", instru: Instruction::ISB,  instru_base_cycle : 5, addring_mode: AddressingMode::ZpAbs },
        InstruAddring { instru_name: "INX", instru: Instruction::INX,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "SBC", instru: Instruction::SBC,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "SBC", instru: Instruction::SBC,  instru_base_cycle : 2, addring_mode: AddressingMode::Imm },
        InstruAddring { instru_name: "CPX", instru: Instruction::CPX,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "SBC", instru: Instruction::SBC,  instru_base_cycle : 4, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "INC", instru: Instruction::INC,  instru_base_cycle : 6, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "ISB", instru: Instruction::ISB,  instru_base_cycle : 6, addring_mode: AddressingMode::Abs },
        InstruAddring { instru_name: "BEQ", instru: Instruction::BEQ,  instru_base_cycle : 2, addring_mode: AddressingMode::Rel },
        InstruAddring { instru_name: "SBC", instru: Instruction::SBC,  instru_base_cycle : 5, addring_mode: AddressingMode::IndY1C },
        InstruAddring { instru_name: "UNK", instru: Instruction::UNK,  instru_base_cycle : 2, addring_mode: AddressingMode::Unk },
        InstruAddring { instru_name: "ISB", instru: Instruction::ISB,  instru_base_cycle : 8, addring_mode: AddressingMode::IndY },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "SBC", instru: Instruction::SBC,  instru_base_cycle : 4, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "INC", instru: Instruction::INC,  instru_base_cycle : 6, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "ISB", instru: Instruction::ISB,  instru_base_cycle : 6, addring_mode: AddressingMode::ZpXI },
        InstruAddring { instru_name: "SED", instru: Instruction::SED,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "SBC", instru: Instruction::SBC,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsYI1C },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 2, addring_mode: AddressingMode::Imp },
        InstruAddring { instru_name: "ISB", instru: Instruction::ISB,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsYI },
        InstruAddring { instru_name: "NOP", instru: Instruction::NOP,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsXI1C },
        InstruAddring { instru_name: "SBC", instru: Instruction::SBC,  instru_base_cycle : 4, addring_mode: AddressingMode::AbsXI1C },
        InstruAddring { instru_name: "INC", instru: Instruction::INC,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsXI },
        InstruAddring { instru_name: "ISB", instru: Instruction::ISB,  instru_base_cycle : 7, addring_mode: AddressingMode::AbsXI },
    ];
}

pub mod apu {
    use std::cell::RefCell;
    use std::rc::Rc;
    use std::usize;

    use bitfield::{Bit, BitMut};

    use crate::common::{Bus, Irq};
    use crate::cpu2a03::cpu;
    use crate::cpu2a03::cycle::{cpu_cycles_add, get_cpu_cycles};
    use crate::cpu2a03::memory::MemMap;

    /* DDLC VVVV
     Duty (D),
    envelope loop / length counter halt (L),
    constant volume (C),
    volume/envelope (V) */
    bitfield! {
        #[derive(Copy, Clone, Default)]
        pub struct EnvelopeReg(u8);
        impl Debug;
        u8;
        pub volume, _: 3,0;
        pub constant_volume, _: 4;
        pub envelope_loop, _: 5; //envelope loop / length counter halt (L)
        pub duty, _: 7,6;
    }
    /* Sweep unit: enabled (E), period (P), negate (N), shift (S)  */
    bitfield! {
        #[derive(Copy, Clone, Default)]
        pub struct SweepReg(u8);
        impl Debug;
        u8;
        pub shift, _: 2,0;
        pub negate, _: 3;
        pub period, _: 6,4;
        pub enabled, _: 7;
    }
    /*  LLLL LTTT 	Length counter load (L), timer high (T)  */
    bitfield! {
        #[derive(Copy, Clone, Default)]
        pub struct LengthCounterReg(u8);
        impl Debug;
        u8;
        pub timer_high, _: 2,0;
        pub length_counter_load, _: 7,3;
    }
    #[derive(Clone, Copy, Default)]
    struct PulseReg {
        envelope: EnvelopeReg,            //$4000 / $4004
        sweep: SweepReg,                  //$4001 / $4005
        timer_low: u8,                    //$4002 / $4006
        length_counter: LengthCounterReg, //$4003 / $4007
    }
    // impl Default for PulseReg {
    //     fn default() -> Self {
    //         Self {
    //             envelope: EnvelopeReg(0),
    //             sweep: SweepReg(0),
    //             timer_low: 0,
    //             length_counter: LengthCounterReg(0),
    //         }
    //     }
    // }

    /* Length counter halt / linear counter control (C), linear counter load (R) */
    bitfield! {
        #[derive(Default)]
        pub struct LinearCounterReg(u8);
        impl Debug;
        u8;
        pub linear_counter , _: 6,0;
        pub control, _: 7;
    }
    #[derive(Default)]
    struct TriangleReg {
        linear_counter: LinearCounterReg, //$4008
        unsed: u8,                        //$4009
        timer_low: u8,                    //$400A
        length_counter: LengthCounterReg, //$400B
    }

    /* M--- PPPP 	Mode flag (M), noise period (P)  */
    bitfield! {
        #[derive(Default)]
        pub struct PeriodReg(u8);
        impl Debug;
        u8;
        pub period , _: 3,0;
        pub mode_flag, _: 7;
    }
    #[derive(Default)]
    struct NoiseReg {
        /* --LC VVVV 	Envelope loop / length counter halt (L), constant volume (C), volume/envelope (V) */
        envelope: EnvelopeReg, //$400C; bit 7, 6 not used.
        unsed: u8,             //$400D
        period: PeriodReg,     //$400E
        length_counter: u8,    //$400F
    }

    /*  IL-- RRRR 	IRQ enable (I), loop (L), frequency (R) */
    bitfield! {
        #[derive(Default)]
        pub struct FrequencyReg(u8);
        impl Debug;
        u8;
        pub frequency , _: 3,0;
        pub loop_en, _: 6;
        pub irq_en, _: 7;
    }
    #[derive(Default)]
    struct DmcReg {
        frequency: FrequencyReg, //$4010
        direct_load: u8,         //$4011
        sample_address: u8,      //$4012
        sample_length: u8,       //$4013
    }

    /*wtite: ---D NT21 	Enable DMC (D), noise (N), triangle (T), and pulse channels (2/1)
    read:  IF-D NT21 	DMC interrupt (I), frame interrupt (F), DMC active (D), length counter > 0 (N/T/2/1) */
    bitfield! {
        #[derive(Default)]
        pub struct ControlReg(u8);
        impl Debug;
        u8;
        pub pulse1_en , _: 0;
        pub pulse2_en , _: 1;
        pub triangle_en, _: 2;
        pub noise_en, _: 3;
        pub dmc_en, _: 4;
    }

    bitfield! {
        #[derive(Default)]
        pub struct FramCntCtrl(u8);
        impl Debug;
        u8;
        pub irq_inhibit_flag, _: 6;
        pub mode, _: 7;
    }

    #[derive(Default)]
    struct Register {
        pulse: [PulseReg; 2],            //$4000~$4003, $4004~$4007
        triangle: TriangleReg,           //$4008~$400b
        noise: NoiseReg,                 //$400c~$400f
        dmc: DmcReg,                     //$4010~$4013
        sta_ctrl: ControlReg,            //$4015
        frame_counter_ctrl: FramCntCtrl, //$4017
    }

    #[derive(Debug, Default)]
    struct Counter {
        counter: u32,
        period: u32,
    }
    impl Counter {
        pub fn count_once<F>(&mut self, mut trig_closure_hook: F)
        where
            F: FnMut(&mut Self),
        {
            if self.counter != 0 {
                self.counter -= 1;
            }
            if self.counter == 0 {
                trig_closure_hook(self);
            }
        }

        pub fn count<F>(&mut self, nticks: u32, mut trig_closure_hook: F)
        where
            F: FnMut(&mut Self, u32),
        {
            let m = self.counter + nticks;
            let n = m / self.period;
            self.counter = m % self.period;
            if n == 0 {
                return;
            }
            trig_closure_hook(self, n);
        }
    }

    struct BlipData {
        period: f32,
        buffer: Vec<u8>,
        smpl_tick: f32,
        tick: u32,
        ampl: u8,
    }

    impl BlipData {
        fn new(cpu_clock_hz: u32, sample_rate: u32) -> Self {
            BlipData {
                period: cpu_clock_hz as f32 / sample_rate as f32,
                buffer: Vec::new(),
                smpl_tick: 0_f32,
                tick: 0,
                ampl: 0,
            }
        }

        fn fill(&mut self, tick: u32, ampl: u8) {
            assert!(tick >= self.tick, "tick shouldn't less than self.tick!!!");

            while (self.smpl_tick + self.period) <= tick as f32 {
                self.buffer.push(self.ampl);
                self.smpl_tick += self.period;
            }
            self.ampl = ampl;
            self.tick = tick;
        }

        fn fill_dclk(&mut self, delta_tick: u32, ampl: u8) {
            let tick = self.tick + delta_tick;
            self.ampl = ampl;
            while (self.smpl_tick + self.period) <= tick as f32 {
                self.buffer.push(self.ampl);
                self.smpl_tick += self.period;
            }
            self.tick = tick;
        }

        fn read_samples(&mut self, buf: &mut [u8]) -> usize {
            let len = buf.len().min(self.buffer.len());
            let d = self.buffer.drain(0..len);
            buf[..len].copy_from_slice(d.as_slice());
            self.tick = (self.tick as f32 - self.smpl_tick) as u32;
            self.smpl_tick = 0_f32;
            len
        }
    }

    pub struct PulseDev {
        freq_div: Counter,
        evelope_div: Counter,
        length_counter: u8,
        sweep_div: Counter,
        dst_period: u32,
        sweep_mute: bool,
        volume: u8,
        blip: BlipData,
        seque_id: u8,
        enable: bool,
    }
    impl PulseDev {
        fn new(cpu_clock_hz: u32, sample_rate: u32) -> Self {
            Self {
                freq_div: Default::default(),
                evelope_div: Default::default(),
                length_counter: 0,
                sweep_div: Default::default(),
                dst_period: 0,
                sweep_mute: false,
                volume: 0,
                blip: BlipData::new(cpu_clock_hz, sample_rate),
                seque_id: 0,
                enable: false,
            }
        }
    }

    pub struct TriangleDev {
        freq_div: Counter,
        length_counter: u8,
        line_counter: u8,
        line_cnt_reload_flag: bool,
        blip: BlipData,
        seque_id: u8,
    }
    impl TriangleDev {
        fn new(cpu_clock_hz: u32, sample_rate: u32) -> Self {
            Self {
                freq_div: Default::default(),
                length_counter: 0,
                line_counter: 0,
                line_cnt_reload_flag: false,
                blip: BlipData::new(cpu_clock_hz, sample_rate),
                seque_id: 0,
            }
        }
    }

    pub struct NoiseDev {
        freq_div: Counter,
        evelope_div: Counter,
        length_counter: u8,
        blip: BlipData,
        volume: u8,
        shift: u16,
    }
    impl NoiseDev {
        fn new(cpu_clock_hz: u32, sample_rate: u32) -> Self {
            Self {
                freq_div: Default::default(),
                evelope_div: Default::default(),
                length_counter: 0,
                blip: BlipData::new(cpu_clock_hz, sample_rate),
                volume: 0,
                shift: 1,
            }
        }
    }

    pub struct DmcDev {
        freq_div: Counter,
        blip: BlipData,
        load_data: u8,
        dac_value: u8,
        sample_address: u16,
        sample_length: u16,
        bit_indx: u8,
        int_flag: bool,
    }
    impl DmcDev {
        fn new(cpu_clock_hz: u32, sample_rate: u32) -> Self {
            Self {
                freq_div: Default::default(),
                blip: BlipData::new(cpu_clock_hz, sample_rate),
                load_data: 0,
                dac_value: 0,
                sample_address: 0,
                sample_length: 0,
                bit_indx: 8,
                int_flag: false,
            }
        }
    }

    pub struct Apu {
        reg: Register,
        set_wave_reg_func_tbl: [fn(&mut Apu, u8); 24],
        cpu_clock_hz: u32,
        frame_status: u16,
        frame_cpu_ticks: u16,
        pulse: [PulseDev; 2],
        triangle: TriangleDev,
        noise: NoiseDev,
        dmc: DmcDev,
        tv_system: u8,
        frame_int_flag: bool,
        last_cpu_tick: u32,
        pub irq: *mut dyn Irq,
        pub bus_to_cpu_mem: *mut dyn Bus,
    }
    fn set_reg_nop(_: &mut Apu, _: u8) {}
    fn set_psulse_0(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.pulse[0].envelope.0 = val;
        if apu_dev.reg.pulse[0].envelope.constant_volume() == false {
            apu_dev.pulse[0].evelope_div.period = apu_dev.reg.pulse[0].envelope.volume() as u32 + 1;
            apu_dev.pulse[0].evelope_div.counter = apu_dev.pulse[0].evelope_div.period;
        } else {
            apu_dev.pulse[0].volume = apu_dev.reg.pulse[0].envelope.volume();
        }
    }
    fn set_psulse_1(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.pulse[0].sweep.0 = val;
        apu_dev.pulse[0].sweep_div.period = apu_dev.reg.pulse[0].sweep.period() as u32 + 1;
        apu_dev.pulse[0].sweep_div.counter = apu_dev.pulse[0].sweep_div.period;
    }
    fn set_psulse_2(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.pulse[0].timer_low = val;
        //timer is updated every APU cycle (i.e., every second CPU cycle)
        apu_dev.pulse[0].freq_div.period =
            ((apu_dev.reg.pulse[0].length_counter.timer_high() as u32) << 8
                | apu_dev.reg.pulse[0].timer_low as u32)
                + 1;
        apu_dev.pulse[0].freq_div.period *= 2;
    }
    fn set_psulse_3(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.pulse[0].length_counter.0 = val;
        apu_dev.pulse[0].freq_div.period =
            ((apu_dev.reg.pulse[0].length_counter.timer_high() as u32) << 8
                | apu_dev.reg.pulse[0].timer_low as u32)
                + 1;
        apu_dev.pulse[0].freq_div.period *= 2;

        if apu_dev.reg.sta_ctrl.pulse1_en() {
            apu_dev.pulse[0].length_counter = LENGTH_COUNTER_TBL
                [apu_dev.reg.pulse[0].length_counter.length_counter_load() as usize];
            apu_dev.pulse[0].evelope_div.counter = 0;
            apu_dev.pulse[0].seque_id = 0;
        }
    }
    fn set_psulse_4(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.pulse[1].envelope.0 = val;
        if apu_dev.reg.pulse[1].envelope.constant_volume() == false {
            apu_dev.pulse[1].evelope_div.period = apu_dev.reg.pulse[1].envelope.volume() as u32 + 1;
            apu_dev.pulse[1].evelope_div.counter = apu_dev.pulse[1].evelope_div.period;
        } else {
            apu_dev.pulse[1].volume = apu_dev.reg.pulse[1].envelope.volume();
        }
    }
    fn set_psulse_5(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.pulse[1].sweep.0 = val;
        apu_dev.pulse[1].sweep_div.period = apu_dev.reg.pulse[1].sweep.period() as u32 + 1;
        apu_dev.pulse[1].sweep_div.counter = apu_dev.pulse[1].sweep_div.period;
    }
    fn set_psulse_6(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.pulse[1].timer_low = val;
        //timer is updated every APU cycle (i.e., every second CPU cycle)
        apu_dev.pulse[1].freq_div.period =
            ((apu_dev.reg.pulse[1].length_counter.timer_high() as u32) << 8
                | apu_dev.reg.pulse[1].timer_low as u32)
                + 1;
        apu_dev.pulse[1].freq_div.period *= 2;
    }
    fn set_psulse_7(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.pulse[1].length_counter.0 = val;
        apu_dev.pulse[1].freq_div.period =
            ((apu_dev.reg.pulse[1].length_counter.timer_high() as u32) << 8
                | apu_dev.reg.pulse[1].timer_low as u32)
                + 1;
        apu_dev.pulse[1].freq_div.period *= 2;
        if apu_dev.reg.sta_ctrl.pulse1_en() {
            apu_dev.pulse[1].length_counter = LENGTH_COUNTER_TBL
                [apu_dev.reg.pulse[1].length_counter.length_counter_load() as usize];
            apu_dev.pulse[1].evelope_div.counter = 0;
            apu_dev.pulse[1].seque_id = 0;
        }
    }
    fn set_triangle_8(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.triangle.linear_counter.0 = val;
        apu_dev.triangle.line_counter = apu_dev.reg.triangle.linear_counter.linear_counter();
    }
    fn set_triangle_10(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.triangle.timer_low = val;
        apu_dev.triangle.freq_div.period =
            ((apu_dev.reg.triangle.length_counter.timer_high() as u32) << 8
                | apu_dev.reg.triangle.timer_low as u32)
                + 1;
    }
    fn set_triangle_11(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.triangle.length_counter.0 = val;
        apu_dev.triangle.freq_div.period =
            ((apu_dev.reg.triangle.length_counter.timer_high() as u32) << 8
                | apu_dev.reg.triangle.timer_low as u32)
                + 1;
        if apu_dev.reg.sta_ctrl.pulse1_en() {
            apu_dev.triangle.length_counter = LENGTH_COUNTER_TBL
                [apu_dev.reg.triangle.length_counter.length_counter_load() as usize];
            apu_dev.triangle.line_cnt_reload_flag = true;
        }
    }
    fn set_noise_12(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.noise.envelope.0 = val;
        if apu_dev.reg.noise.envelope.constant_volume() == false {
            apu_dev.noise.evelope_div.period = apu_dev.reg.noise.envelope.volume() as u32 + 1;
            apu_dev.noise.evelope_div.counter = apu_dev.noise.evelope_div.period;
            apu_dev.noise.volume = 15;
        } else {
            apu_dev.noise.volume = apu_dev.reg.noise.envelope.volume();
        }
    }
    fn set_noise_14(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.noise.period.0 = val;
        apu_dev.noise.freq_div.period = NOISE_FREQUENCE_TBL[apu_dev.tv_system as usize]
            [apu_dev.reg.noise.period.period() as usize]
            as u32;
    }
    fn set_noise_15(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.noise.length_counter = val >> 3;
        if apu_dev.reg.sta_ctrl.noise_en() {
            apu_dev.noise.length_counter =
                LENGTH_COUNTER_TBL[apu_dev.reg.noise.length_counter as usize];
            apu_dev.noise.evelope_div.counter = 0;
        }
    }
    fn set_dmc_16(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.dmc.frequency.0 = val;
        apu_dev.dmc.freq_div.period = DMC_FREQUENCE_TBL[apu_dev.tv_system as usize]
            [apu_dev.reg.dmc.frequency.frequency() as usize]
            as u32;
        if apu_dev.reg.dmc.frequency.irq_en() == false {
            apu_dev.dmc.int_flag = false;
        }
    }
    fn set_dmc_17(apu_dev: &mut Apu, val: u8) {
        apu_dev.dmc.load_data = val & 0x7f;
    }
    fn set_dmc_18(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.dmc.sample_address = val;
        apu_dev.dmc.sample_address = ((val as u16) << 6) | 0xc000;
    }
    fn set_dmc_19(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.dmc.sample_length = val;
        apu_dev.dmc.sample_length = ((val as u16) << 4) | 1;
    }
    fn set_status_ctrl_21(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.sta_ctrl.0 = val;

        if apu_dev.reg.sta_ctrl.pulse1_en() == false {
            apu_dev.pulse[0].length_counter = 0;
            apu_dev.pulse[0].enable = false;
        } else {
            apu_dev.pulse[0].enable = true;
        }
        if apu_dev.reg.sta_ctrl.pulse2_en() == false {
            apu_dev.pulse[1].length_counter = 0;
            apu_dev.pulse[1].enable = false;
        } else {
            apu_dev.pulse[1].enable = true;
        }
        if apu_dev.reg.sta_ctrl.triangle_en() == false {
            apu_dev.triangle.length_counter = 0;
        }
        if apu_dev.reg.sta_ctrl.noise_en() == false {
            apu_dev.noise.length_counter = 0;
        }
        if apu_dev.reg.sta_ctrl.dmc_en() == false {
            apu_dev.dmc.sample_length = 0;
        }
        apu_dev.dmc.int_flag = false;
    }
    fn set_frame_counter_23(apu_dev: &mut Apu, val: u8) {
        apu_dev.reg.frame_counter_ctrl.0 = val;
        if apu_dev.reg.frame_counter_ctrl.irq_inhibit_flag() {
            apu_dev.frame_int_flag = false;
        }
        // 5 步模式会立刻产生一个 1/2 和 1/4 时钟信号
        if apu_dev.reg.frame_counter_ctrl.mode() {
            // Length counter and sweep
            apu_dev.trig_pulse_length(0);
            apu_dev.trig_pulse_length(1);
            apu_dev.trig_pulse_sweep(0);
            apu_dev.trig_pulse_sweep(1);
            apu_dev.trig_triangle_length();
            apu_dev.trig_noise_length();
            // Envelope and linear counter
            apu_dev.trig_pulse_envelope(0);
            apu_dev.trig_pulse_envelope(1);
            apu_dev.trig_triangle_line();
        }
        apu_dev.frame_status = 0;
        apu_dev.frame_cpu_ticks = 0;
    }

    impl Bus for Apu {
        fn read(&mut self, _addr: u16) -> u8 {
            let mut val = 0_u8;

            self.irq_ack();

            if self.pulse[0].length_counter != 0 {
                val.set_bit(0, true);
            }
            if self.pulse[1].length_counter != 0 {
                val.set_bit(1, true);
            }
            if self.triangle.length_counter != 0 {
                val.set_bit(2, true);
            }
            if self.noise.length_counter != 0 {
                val.set_bit(3, true);
            }
            if self.dmc.sample_length != 0 {
                val.set_bit(4, true);
            }
            if self.frame_int_flag {
                val.set_bit(6, true);
            }
            if self.dmc.int_flag {
                val.set_bit(7, true);
            }
            self.frame_int_flag = false;

            val
        }

        fn write(&mut self, addr: u16, val: u8) {
            self.set_wave_reg_func_tbl[(addr & 0x1f) as usize](self, val);
        }
    }

    impl Apu {
        // tv_system: 0 NTSC, 1 PAL
        pub fn new(
            cpu_clock_hz: u32,
            sample_rate: u32,
            tv_system: u8,
            irq: *mut dyn Irq,
            mem_bus: *mut dyn Bus,
        ) -> Apu {
            let mut a = Apu {
                reg: Default::default(),
                #[rustfmt::skip]
                set_wave_reg_func_tbl: [
                    set_psulse_0, set_psulse_1, set_psulse_2, set_psulse_3,
                    set_psulse_4, set_psulse_5, set_psulse_6, set_psulse_7,
                    set_triangle_8, set_reg_nop, set_triangle_10, set_triangle_11,
                    set_noise_12, set_reg_nop, set_noise_14, set_noise_15,
                    set_dmc_16, set_dmc_17, set_dmc_18, set_dmc_19,
                    set_reg_nop, set_status_ctrl_21, set_reg_nop, set_frame_counter_23,
                ],
                cpu_clock_hz,
                frame_status: 0,
                frame_cpu_ticks: 0,
                pulse: [
                    PulseDev::new(cpu_clock_hz, sample_rate),
                    PulseDev::new(cpu_clock_hz, sample_rate),
                ],
                triangle: TriangleDev::new(cpu_clock_hz, sample_rate),
                noise: NoiseDev::new(cpu_clock_hz, sample_rate),
                dmc: DmcDev::new(cpu_clock_hz, sample_rate),
                tv_system,
                frame_int_flag: false,
                last_cpu_tick: 0,
                irq,
                bus_to_cpu_mem: mem_bus,
            };
            a.frame_cpu_ticks = 12;
            a
        }

        fn cpu_mem_read(&mut self, addr: u16) -> u8 {
            unsafe { (*self.bus_to_cpu_mem).read(addr) }
        }

        fn irq_ack(&mut self) {
            unsafe {
                (*self.irq).irq_ack();
            }
        }

        fn irq_req(&mut self) {
            unsafe {
                (*self.irq).irq_req();
            }
        }
        /*********************** Pulse channel **************************
                          Sweep -----> Timer
                            |            |
                            |            |
                            |            v
                            |        Sequencer   Length Counter
                            |            |             |
                            |            |             |
                            v            v             v
        Envelope --------> Gate ------> Gate -------> Gate ---> (to mixer)
        *******************************************************************/
        #[inline(always)]
        fn trig_pulse_envelope(&mut self, pulse_id: u8) {
            let reg = &self.reg.pulse[pulse_id as usize].envelope;
            let pulse = &mut self.pulse[pulse_id as usize];

            if reg.constant_volume() == false {
                let volume = &mut pulse.volume;
                pulse.evelope_div.count_once(|div| {
                    div.counter = div.period;
                    if *volume == 0 {
                        if reg.envelope_loop() == true {
                            *volume = 15;
                        }
                    } else {
                        *volume -= 1;
                    }
                });
            }
        }
        #[inline(always)]
        fn trig_pulse_length(&mut self, pulse_id: u8) {
            let reg = &self.reg.pulse[pulse_id as usize].envelope;
            let pulse = &mut self.pulse[pulse_id as usize];

            if pulse.enable && (reg.envelope_loop() == false) {
                if pulse.length_counter > 0 {
                    pulse.length_counter -= 1;
                }
            }
        }
        #[inline(always)]
        fn trig_pulse_sweep(&mut self, pulse_id: u8) {
            let pulse = &mut self.pulse[pulse_id as usize];
            if pulse.sweep_div.period == 0 {
                return;
            }

            let reg = &self.reg.pulse[pulse_id as usize].sweep;
            pulse.sweep_div.count_once(|timer| {
                timer.counter = timer.period;
                let result = pulse.dst_period >> reg.shift();
                pulse.dst_period = if reg.negate() {
                    pulse
                        .dst_period
                        .saturating_sub(result + 1 - pulse_id as u32)
                } else {
                    pulse.dst_period.saturating_add(result)
                };
                if (pulse.freq_div.period < 8 * 2) || (pulse.dst_period > 0x7ff) {
                    pulse.sweep_mute = true;
                } else {
                    pulse.sweep_mute = false;
                    if reg.enabled() == true && reg.shift() != 0 {
                        pulse.freq_div.period = pulse.dst_period * 2;
                        pulse.freq_div.counter = pulse.freq_div.period;
                    }
                }
            });
        }
        #[inline(always)]
        fn trig_pulse_timer(&mut self, pulse_id: u8, cpu_ticks: u16) {
            let reg = &self.reg.pulse[pulse_id as usize];
            let pulse = &mut self.pulse[pulse_id as usize];
            if (pulse.enable == false)
                || (pulse.sweep_mute == true)
                || (pulse.length_counter == 0)
                || (pulse.freq_div.period < 8 * 2)
                || (pulse.freq_div.period > 0x7ff * 2)
            {
                return;
            }

            let blip = &mut pulse.blip;
            let period = pulse.freq_div.period;
            let vl = pulse.volume;
            let seque_id = &mut pulse.seque_id;
            let duty_id = reg.envelope.duty() as usize;
            //timer
            pulse.freq_div.count(cpu_ticks as u32, |_, n| {
                for _ in 0..n {
                    blip.fill_dclk(
                        period,
                        PULSE_SEQUENCE_DUTY_TBL[duty_id][*seque_id as usize] * vl,
                    );
                    *seque_id = (*seque_id + 1) & 0x7;
                }
            });
        }

        /*********************** Triangle channel **************************
              Linear Counter   Length Counter
                    |                |
                    v                v
        Timer ---> Gate ----------> Gate ---> Sequencer ---> (to mixer)
        *******************************************************************/
        #[inline(always)]
        fn trig_triangle_length(&mut self) {
            let triangle = &mut self.triangle;
            let apu_reg: &Register = &self.reg;

            // length count
            if apu_reg.sta_ctrl.triangle_en()
                && (apu_reg.triangle.linear_counter.control() == false)
            {
                if triangle.length_counter > 0 {
                    triangle.length_counter -= 1;
                }
            }
        }
        #[inline(always)]
        fn trig_triangle_line(&mut self) {
            let triangle = &mut self.triangle;
            let apu_reg: &Register = &self.reg;

            if apu_reg.sta_ctrl.triangle_en() {
                if apu_reg.triangle.linear_counter.control() == false {
                    triangle.line_cnt_reload_flag = false;
                    if triangle.line_counter > 0 {
                        triangle.line_counter -= 1;
                    }
                }
                if triangle.line_cnt_reload_flag {
                    triangle.line_counter = apu_reg.triangle.length_counter.length_counter_load();
                }
            }
        }
        #[inline(always)]
        fn trig_triangle_timer(&mut self, cpu_ticks: u16) {
            let triangle = &mut self.triangle;
            if triangle.freq_div.period == 0
            // || (triangle.freq_div.period <= 2)
            || (triangle.freq_div.period > 0x7ff)
            {
                return;
            }
            let apu_reg: &Register = &self.reg;
            let mute = if apu_reg.sta_ctrl.triangle_en() == false
                || (triangle.line_counter == 0)
                || (triangle.length_counter == 0)
            {
                true
            } else {
                false
            };

            let blip = &mut triangle.blip;
            let period = triangle.freq_div.period;

            /* timer */
            // Silencing the triangle channel merely halts it. It will continue to output its last value rather than 0.
            if mute {
                triangle.freq_div.count(cpu_ticks as u32, |_, n| {
                    for _ in 0..n {
                        blip.fill_dclk(period, TRIANGLE_SEQUENCE_TBL[triangle.seque_id as usize]);
                    }
                });
            } else {
                triangle.freq_div.count(cpu_ticks as u32, |_, n| {
                    for _ in 0..n {
                        blip.fill_dclk(period, TRIANGLE_SEQUENCE_TBL[triangle.seque_id as usize]);
                        triangle.seque_id = (triangle.seque_id + 1) & 0x1f;
                    }
                });
            }
        }

        /*********************** Noise channel **************************
                Timer --> Shift Register   Length Counter
                                |                |
                                v                v
            Envelope -------> Gate ----------> Gate --> (to mixer)
        *******************************************************************/
        #[inline(always)]
        fn trig_noise_envelope(&mut self) {
            let apu_reg: &Register = &self.reg;
            let noise = &mut self.noise;

            if apu_reg.noise.envelope.constant_volume() == false {
                let volume = &mut noise.volume;
                noise.evelope_div.count_once(|div| {
                    div.counter = div.period;
                    if *volume == 0 {
                        if apu_reg.noise.envelope.envelope_loop() == true {
                            *volume = 15;
                        }
                    } else {
                        *volume -= 1;
                    }
                });
            }
        }
        #[inline(always)]
        fn trig_noise_length(&mut self) {
            let noise = &mut self.noise;
            let apu_reg: &Register = &self.reg;

            // length count
            if apu_reg.sta_ctrl.noise_en() && (apu_reg.noise.envelope.envelope_loop() == false) {
                if noise.length_counter > 0 {
                    noise.length_counter -= 1;
                }
            }
        }
        #[inline(always)]
        fn trig_noise_timer(&mut self, cpu_ticks: u16) {
            let noise = &mut self.noise;
            let apu_reg: &Register = &self.reg;
            if (apu_reg.sta_ctrl.noise_en() == false)
                || (noise.length_counter == 0)
                || (noise.freq_div.period == 0)
            {
                return;
            }

            let blip = &mut noise.blip;
            let period = noise.freq_div.period as u32;
            let vl = noise.volume;

            //timer
            noise.freq_div.count(cpu_ticks as u32, |_, n| {
                for _ in 0..n {
                    let feedback = if apu_reg.noise.period.mode_flag() {
                        //short mode
                        ((noise.shift >> 6) ^ noise.shift) & 0x1
                    } else {
                        ((noise.shift >> 1) ^ noise.shift) & 0x1
                    };
                    noise.shift = (noise.shift >> 1) | (feedback << 14);
                    let ampl = if noise.shift & 0x1 == 0 { 1 } else { 0 } * vl;
                    blip.fill_dclk(period, ampl);
                }
            });
        }

        /******************************* DMC channel **********************************
                                     Timer
                                       |
                                       v
            Reader ---> Buffer ---> Shifter ---> Output level ---> (to the mixer)
        *******************************************************************************/
        #[inline(always)]
        fn trig_dmc_timer(&mut self, cpu_ticks: u16) {
            let dmc = &mut self.dmc;
            let apu_reg: &Register = &self.reg;

            if (dmc.freq_div.period == 0)
                || ((apu_reg.sta_ctrl.dmc_en() == false || dmc.sample_length == 0)
                    && dmc.bit_indx == 8)
            {
                return;
            }

            let mem_bus = self.bus_to_cpu_mem;
            let blip = &mut dmc.blip;
            let period = dmc.freq_div.period;

            //timer
            dmc.freq_div.count(cpu_ticks as u32, |_, n| {
                let mut ampl;
                for _ in 0..n {
                    ampl = 0;
                    if apu_reg.sta_ctrl.dmc_en() == true {
                        if dmc.bit_indx == 8 {
                            if dmc.sample_length != 0 {
                                //read data from prgrom
                                dmc.load_data = unsafe { (*mem_bus).read(dmc.sample_address) };
                                // TODO: spend cpu cycles
                                //The address is incremented; if it exceeds $FFFF, it is wrapped around to $8000.
                                dmc.sample_address = dmc.sample_address.saturating_add(1) | 0x8000;
                                dmc.sample_length -= 1;
                                dmc.bit_indx = 0;
                            } else {
                                if apu_reg.dmc.frequency.loop_en() {
                                    dmc.sample_length =
                                        ((apu_reg.dmc.sample_length as u16) << 4) | 1;
                                    dmc.sample_address =
                                        ((apu_reg.dmc.sample_address as u16) << 6) | 0xc000;
                                } else if apu_reg.dmc.frequency.irq_en() {
                                    dmc.int_flag = true;
                                }
                            }
                        }
                    }
                    if dmc.bit_indx < 8 {
                        if dmc.load_data.bit(dmc.bit_indx as usize) {
                            if dmc.dac_value <= 125 {
                                dmc.dac_value += 2;
                            }
                        } else {
                            dmc.dac_value = dmc.dac_value.saturating_sub(2);
                        }
                        dmc.bit_indx += 1;
                        ampl = dmc.dac_value;
                    }
                    blip.fill_dclk(period, ampl);
                }
            });
        }

        /**************************************************************************
            output = pulse_out + tnd_out

                            95.88
            pulse_out = ------------------------------------
                        (8128 / (pulse1 + pulse2)) + 100

                                                159.79
            tnd_out = -------------------------------------------------------------
                                                 1
                      ----------------------------------------------------- + 100
                          (triangle / 8227) + (noise / 12241) + (dmc / 22638)
        */
        pub fn mix(&mut self, sample_buf: &mut Vec<f32>) {
            let blip_len = [
                self.pulse[0].blip.buffer.len(),
                self.pulse[1].blip.buffer.len(),
                self.triangle.blip.buffer.len(),
                self.noise.blip.buffer.len(),
                self.dmc.blip.buffer.len(),
            ];
            if blip_len == [0, 0, 0, 0, 0] {
                return;
            }
            // println!("blip_len={:?}", blip_len);

            let mut len: usize = std::usize::MAX;
            for v in blip_len.iter() {
                if *v != 0 {
                    len = len.min(*v);
                }
            }

            let mut buf1: Vec<u8> = Vec::new();
            let mut buf2: Vec<u8> = Vec::new();
            let indx = sample_buf.len();
            // println!("len={},indx={:?}", len, indx);
            /************************ pulse out **************************/
            buf1.resize(len, 0);
            buf2.resize(len, 0);

            self.pulse[0].blip.read_samples(&mut buf1);
            self.pulse[1].blip.read_samples(&mut buf2);
            for i in 0..len {
                // f32 divisor can be 0.
                sample_buf.push(95.88 / (8128.0 / (buf1[i] + buf2[i]) as f32 + 100.0));
            }

            /************************ tnd out **************************/
            let mut buf3: Vec<u8> = Vec::new();
            buf3.resize(len, 0);
            self.dmc.blip.read_samples(&mut buf3);
            if blip_len[2] != 0 && blip_len[3] != 0 {
                self.triangle.blip.read_samples(&mut buf1);
                self.noise.blip.read_samples(&mut buf2);
                for i in 0..len {
                    let tnd_out = 159.79
                        / (1.0
                            / (buf1[i] as f32 / 8227.0
                                + buf2[i] as f32 / 12241.0
                                + buf3[i] as f32 / 22638.0)
                            + 100.0);
                    sample_buf[indx + i] += tnd_out;
                }
            } else if blip_len[2] != 0 {
                self.triangle.blip.read_samples(&mut buf1);
                for i in 0..len {
                    let tnd_out = 159.79
                        / (1.0 / (buf1[i] as f32 / 8227.0 + buf3[i] as f32 / 22638.0) + 100.0);
                    sample_buf[indx + i] += tnd_out;
                }
            } else {
                self.noise.blip.read_samples(&mut buf2);
                for i in 0..len {
                    let tnd_out = 159.79
                        / (1.0 / (buf2[i] as f32 / 12241.0 + buf3[i] as f32 / 22638.0) + 100.0);
                    sample_buf[indx + i] += tnd_out;
                }
            }
            // println!("sample_buf={:?}", sample_buf);
        }

        /**************************************************************************
           pulse_table [n] = 95.52 / (8128.0 / n + 100)
           pulse_out = pulse_table [pulse1 + pulse2]

           tnd_table [n] = 163.67 / (24329.0 / n + 100)
           tnd_out = tnd_table [3 * triangle + 2 * noise + dmc]

           output = pulse_out + tnd_out
        */
        pub fn mix_fast(&mut self, sample_buf: &mut Vec<f32>) {
            let blip_len = [
                self.pulse[0].blip.buffer.len(),
                self.pulse[1].blip.buffer.len(),
                self.triangle.blip.buffer.len(),
                self.noise.blip.buffer.len(),
                self.dmc.blip.buffer.len(),
            ];
            if blip_len == [0, 0, 0, 0, 0] {
                return;
            }

            let mut len: usize = std::usize::MAX;
            for v in blip_len.iter() {
                if *v != 0 {
                    len = len.min(*v);
                }
            }

            let mut buf1: Vec<u8> = Vec::new();
            let mut buf2: Vec<u8> = Vec::new();
            let indx = sample_buf.len();
            // println!("len={},indx={:?}", len, indx);
            buf1.resize(len, 0);
            buf2.resize(len, 0);

            /************************ pulse out **************************/
            self.pulse[0].blip.read_samples(&mut buf1);
            self.pulse[1].blip.read_samples(&mut buf2);
            for i in 0..len {
                sample_buf.push(MIX_PULSE_TBL[(buf1[i] + buf2[i]) as usize]);
            }

            /************************ tnd out **************************/
            let mut buf3: Vec<u8> = Vec::new();
            buf3.resize(len, 0);
            self.dmc.blip.read_samples(&mut buf3);

            if blip_len[2] != 0 && blip_len[3] != 0 {
                self.triangle.blip.read_samples(&mut buf1);
                self.noise.blip.read_samples(&mut buf2);
                for i in 0..len {
                    sample_buf[indx + i] +=
                        MIX_TND_TBL[(buf1[i] * 3 + buf2[i] * 2 + buf3[i]) as usize];
                }
            } else if blip_len[2] != 0 {
                self.triangle.blip.read_samples(&mut buf1);
                for i in 0..len {
                    sample_buf[indx + i] += MIX_TND_TBL[(buf1[i] * 3 + buf2[i]) as usize];
                }
            } else {
                self.noise.blip.read_samples(&mut buf2);
                for i in 0..len {
                    sample_buf[indx + i] += MIX_TND_TBL[(buf2[i] * 2 + buf3[i]) as usize];
                }
            }
            // println!("sample_buf={:?}", sample_buf);
        }

        /*  mode 0:    mode 1:       function
            ---------  -----------  -----------------------------
            - - - f    - - - - -    IRQ (if bit 6 is clear)
            - l - l    - l - - l    Length counter and sweep
            e e e e    e e e - e    Envelope and linear counter
        */
        #[inline]
        fn frame_counter_trig_mode0(&mut self, cpu_cycles: u16) {
            self.frame_cpu_ticks += cpu_cycles;

            match self.frame_status {
                num @ (0 | 2)
                    if self.frame_cpu_ticks
                        > FRAME_PEROID_MODE0[self.tv_system as usize][num as usize] =>
                {
                    self.trig_pulse_envelope(0);
                    self.trig_pulse_envelope(1);
                    self.trig_triangle_line();

                    self.frame_status += 1;
                }
                1 if self.frame_cpu_ticks > FRAME_PEROID_MODE0[self.tv_system as usize][1] => {
                    self.trig_pulse_length(0);
                    self.trig_pulse_length(1);
                    self.trig_pulse_sweep(0);
                    self.trig_pulse_sweep(1);
                    self.trig_triangle_length();
                    self.trig_noise_length();

                    self.trig_pulse_envelope(0);
                    self.trig_pulse_envelope(1);
                    self.trig_triangle_line();

                    self.frame_status = 2;
                }
                3 if self.frame_cpu_ticks == FRAME_PEROID_MODE0[self.tv_system as usize][3] => {
                    //IRQ
                    if self.reg.frame_counter_ctrl.irq_inhibit_flag() == false {
                        self.frame_int_flag = true;
                    }
                }
                3 if self.frame_cpu_ticks > FRAME_PEROID_MODE0[self.tv_system as usize][3] => {
                    self.trig_pulse_length(0);
                    self.trig_pulse_length(1);
                    self.trig_pulse_sweep(0);
                    self.trig_pulse_sweep(1);
                    self.trig_triangle_length();
                    self.trig_noise_length();

                    self.trig_pulse_envelope(0);
                    self.trig_pulse_envelope(1);
                    self.trig_triangle_line();
                    //IRQ
                    if self.reg.frame_counter_ctrl.irq_inhibit_flag() == false {
                        self.frame_int_flag = true;
                    }
                    self.frame_status = 4;
                }
                4 if self.frame_cpu_ticks
                    >= (FRAME_PEROID_MODE0[self.tv_system as usize][3] + 3) =>
                {
                    self.frame_status = 0;
                    self.frame_cpu_ticks -= FRAME_PEROID_MODE0[self.tv_system as usize][3] - 1;
                    if self.frame_int_flag == true {
                        self.irq_req();
                    }
                }
                _ => {}
            };
        }

        #[inline]
        fn frame_counter_trig_mode1(&mut self, cpu_cycles: u16) {
            self.frame_cpu_ticks += cpu_cycles;

            match self.frame_status {
                num @ (0 | 2)
                    if self.frame_cpu_ticks
                        > FRAME_PEROID_MODE1[self.tv_system as usize][num as usize] =>
                {
                    self.trig_pulse_envelope(0);
                    self.trig_pulse_envelope(1);
                    self.trig_triangle_line();

                    self.frame_status += 1;
                }
                num @ (1 | 4)
                    if self.frame_cpu_ticks
                        > FRAME_PEROID_MODE1[self.tv_system as usize][num as usize] =>
                {
                    self.trig_pulse_length(0);
                    self.trig_pulse_length(1);
                    self.trig_pulse_sweep(0);
                    self.trig_pulse_sweep(1);
                    self.trig_triangle_length();
                    self.trig_noise_length();

                    self.trig_pulse_envelope(0);
                    self.trig_pulse_envelope(1);
                    self.trig_triangle_line();

                    if self.frame_status == 4 {
                        self.frame_status = 0;
                        self.frame_cpu_ticks -=
                            FRAME_PEROID_MODE1[self.tv_system as usize][num as usize] - 1;
                    } else {
                        self.frame_status += 1;
                    }
                }
                3 if self.frame_cpu_ticks > FRAME_PEROID_MODE1[self.tv_system as usize][3] => {
                    self.frame_status = 4;
                }
                _ => {}
            };
        }

        pub fn frame_counter_trig(&mut self, cpu_cycles: u16) {
            /*  mode 0:    mode 1:       function
                ---------  -----------  -----------------------------
                - - - f    - - - - -    IRQ (if bit 6 is clear)
                - l - l    - l - - l    Length counter and sweep
                e e e e    e e e - e    Envelope and linear counter
            */
            if self.reg.frame_counter_ctrl.mode() {
                self.frame_counter_trig_mode1(cpu_cycles);
            } else {
                self.frame_counter_trig_mode0(cpu_cycles);
            };

            self.trig_pulse_timer(0, cpu_cycles);
            self.trig_pulse_timer(1, cpu_cycles);
            self.trig_triangle_timer(cpu_cycles);
            self.trig_noise_timer(cpu_cycles);
            self.trig_dmc_timer(cpu_cycles);
        }
    }

    const LENGTH_COUNTER_TBL: [u8; 32] = [
        10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14, 12, 16, 24, 18, 48, 20, 96,
        22, 192, 24, 72, 26, 16, 28, 32, 30,
    ];
    const PULSE_SEQUENCE_DUTY_TBL: [[u8; 8]; 4] = [
        [0, 1, 0, 0, 0, 0, 0, 0],
        [0, 1, 1, 0, 0, 0, 0, 0],
        [0, 1, 1, 1, 1, 0, 0, 0],
        [1, 0, 0, 1, 1, 1, 1, 1],
    ];
    const TRIANGLE_SEQUENCE_TBL: [u8; 32] = [
        15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
        12, 13, 14, 15,
    ];
    const NOISE_FREQUENCE_TBL: [[u16; 16]; 2] = [
        //0 NTSC
        [
            4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
        ],
        //1 PAL
        [
            4, 8, 14, 30, 60, 88, 118, 148, 188, 236, 354, 472, 708, 944, 1890, 3778,
        ],
    ];
    const DMC_FREQUENCE_TBL: [[u16; 16]; 2] = [
        //0 NTSC
        [
            428, 380, 340, 320, 286, 254, 226, 214, 190, 160, 142, 128, 106, 84, 72, 54,
        ],
        //1 PAL
        [
            398, 354, 316, 298, 276, 236, 210, 198, 176, 148, 132, 118, 98, 78, 66, 50,
        ],
    ];
    #[rustfmt::skip]
    const MIX_PULSE_TBL: [f32; 31] = [
        0.0, 0.011609139, 0.02293948, 0.034000948,
        0.044803, 0.05535466, 0.06566453, 0.07574082,
        0.0855914, 0.09522375, 0.10464504, 0.11386215,
        0.12288164, 0.1317098, 0.14035264, 0.14881596,
        0.15710525, 0.16522588, 0.17318292, 0.18098126,
        0.18862559, 0.19612046, 0.20347017, 0.21067894,
        0.21775076, 0.2246895, 0.23149887, 0.23818247,
        0.24474378, 0.25118607, 0.25751257
    ];

    #[rustfmt::skip]
    const MIX_TND_TBL: [f32; 203] = [
        0.0, 0.006699824, 0.01334502, 0.019936256,
        0.02647418, 0.032959443, 0.039392676, 0.0457745,
        0.052105535, 0.05838638, 0.064617634, 0.07079987,
        0.07693369, 0.08301962, 0.08905826, 0.095050134,
        0.100995794, 0.10689577, 0.11275058, 0.118560754,
        0.12432679, 0.13004918, 0.13572845, 0.14136505,
        0.1469595, 0.15251222, 0.1580237, 0.1634944,
        0.16892476, 0.17431524, 0.17966628, 0.1849783,
        0.19025174, 0.19548698, 0.20068447, 0.20584463,
        0.21096781, 0.21605444, 0.22110492, 0.2261196,
        0.23109888, 0.23604311, 0.24095272, 0.245828,
        0.25066936, 0.2554771, 0.26025164, 0.26499328,
        0.26970237, 0.27437922, 0.27902418, 0.28363758,
        0.28821972, 0.29277095, 0.29729152, 0.3017818,
        0.3062421, 0.31067267, 0.31507385, 0.31944588,
        0.32378912, 0.32810378, 0.3323902, 0.3366486,
        0.3408793, 0.34508255, 0.34925863, 0.35340777,
        0.35753027, 0.36162636, 0.36569634, 0.36974037,
        0.37375876, 0.37775174, 0.38171956, 0.38566244,
        0.38958064, 0.39347437, 0.39734384, 0.4011893,
        0.405011, 0.40880907, 0.41258383, 0.41633546,
        0.42006415, 0.42377013, 0.4274536, 0.43111476,
        0.43475384, 0.43837097, 0.44196644, 0.4455404,
        0.449093, 0.45262453, 0.45613506, 0.4596249,
        0.46309412, 0.46654293, 0.46997157, 0.47338015,
        0.47676894, 0.48013794, 0.48348752, 0.4868177,
        0.49012873, 0.4934207, 0.49669388, 0.49994832,
        0.50318426, 0.50640184, 0.5096012, 0.51278245,
        0.51594585, 0.5190914, 0.5222195, 0.52533007,
        0.52842325, 0.5314993, 0.53455836, 0.5376005,
        0.54062593, 0.5436348, 0.54662704, 0.54960304,
        0.55256283, 0.55550647, 0.5584343, 0.56134623,
        0.5642425, 0.56712323, 0.5699885, 0.5728384,
        0.5756732, 0.57849294, 0.5812977, 0.5840876,
        0.5868628, 0.58962345, 0.59236956, 0.59510136,
        0.5978189, 0.6005223, 0.6032116, 0.605887,
        0.60854864, 0.6111966, 0.6138308, 0.61645156,
        0.619059, 0.62165314, 0.624234, 0.62680185,
        0.6293567, 0.63189864, 0.6344277, 0.6369442,
        0.63944805, 0.64193934, 0.64441824, 0.64688486,
        0.6493392, 0.6517814, 0.6542115, 0.65662974,
        0.65903604, 0.6614306, 0.6638134, 0.66618466,
        0.66854435, 0.6708926, 0.67322946, 0.67555505,
        0.67786944, 0.68017274, 0.68246496, 0.6847462,
        0.6870166, 0.6892762, 0.69152504, 0.6937633,
        0.6959909, 0.69820803, 0.7004148, 0.7026111,
        0.7047972, 0.7069731, 0.7091388, 0.7112945,
        0.7134401, 0.7155759, 0.7177018, 0.7198179,
        0.72192425, 0.72402096, 0.726108, 0.72818565,
        0.7302538, 0.73231256, 0.73436195, 0.7364021,
        0.7384331, 0.7404549, 0.7424676
    ];

    const FRAME_PEROID_MODE0: [[u16; 4]; 2] = [
        [7457, 14913 + 2, 22371, 29830 + 1], //NTSC
        [8313, 16627, 24939, 33254],         //PAL
    ];
    const FRAME_PEROID_MODE1: [[u16; 5]; 2] = [
        [7457, 14913 + 2, 22371, 29828, 37281 + 2], //NTSC
        [8313, 16627, 24939, 33252, 41565],         //PAL
    ];
}
