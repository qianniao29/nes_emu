#![allow(dead_code)]

pub mod cpu {
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

    $4020 	$1FDF 		Expansion ROM
    $6000 	$2000 		SRAM
    $8000 	$4000 		PRG-ROM
    $C000 	$4000 		PRG-ROM
    */

    pub const NMI_VECT_ADDR: u16 = 0xFFFA;
    pub const REST_VECT_ADDR: u16 = 0xFFFC;
    pub const BRK_VECT_ADDR: u16 = 0xFFFE;

    pub const KEY_A: usize = 0;
    pub const KEY_B: usize = 1;
    pub const KEY_SELECT: usize = 2;
    pub const KEY_START: usize = 3;
    pub const KEY_UP: usize = 4;
    pub const KEY_DOWN: usize = 5;
    pub const KEY_LEFT: usize = 6;
    pub const KEY_RIGHT: usize = 7;

    static mut CPU_CYCLES: u32 = 0;

    pub fn cpu_cycles_reset() {
        unsafe {
            CPU_CYCLES = 0;
        }
    }

    pub fn cpu_cycles_add(cnt: u32) {
        unsafe {
            CPU_CYCLES = CPU_CYCLES.wrapping_add(cnt);
        }
    }

    pub fn get_cpu_cycles() -> u32 {
        unsafe { CPU_CYCLES }
    }

    pub struct MemMap<'a> {
        pub ram: [u8; 0x800],
        pub ppu_reg: ppu::Register,
        pub apu_reg: [u8; 0x18],
        pub sram: [u8; 0x2000], // save RAM
        pub prg_rom: [&'a [u8]; 4],
        pub key: [[u8; 8]; 2],
        pub key_indx: [usize; 2],
        pub ppu_mem: ppu::MemMap,
    }

    impl MemMap<'_> {
        pub fn new() -> Self {
            MemMap {
                ram: [0; 0x800],
                ppu_reg: ppu::Register::new(),
                apu_reg: [0; 0x18],
                key: [[0; 8]; 2],
                key_indx: [0; 2],
                sram: [0; 0x2000],
                prg_rom: Default::default(),
                ppu_mem: ppu::MemMap::new(),
            }
        }
        pub fn read_memeory(&mut self, addr: u16) -> u8 {
            let i: usize = addr.into();
            let j: usize = i >> 13;

            if addr & 0x8000 == 0x8000 {
                // 高一位为 1, [$8000, $10000) 程序 PRG-ROM 区
                self.prg_rom[j & 3][i & 0x1fff]
            } else {
                match j {
                    // 高三位为 0: [$0000, $2000): 系统主内存，4 次镜像
                    0 => self.ram[i & 0x7ff],
                    // 高三位为 1, [$2000, $4000): PPU 寄存器，8 字节步进镜像
                    1 => self.ppu_reg.read(&mut self.ppu_mem, addr),
                    // 高三位为 2, [$4000, $6000): pAPU 寄存器 扩展 ROM 区
                    2 => {
                        if addr & 0x1f == 0x16 {
                            let val = self.key[0][self.key_indx[0]];
                            self.key_indx[0] += 1;
                            if self.key_indx[0] > 7 {
                                self.key_indx[0] = 0;
                            }
                            val
                        } else if addr & 0x1f == 0x17 {
                            let val = self.key[1][self.key_indx[1]];
                            self.key_indx[1] += 1;
                            if self.key_indx[1] > 7 {
                                self.key_indx[1] = 0;
                            }
                            val
                        } else {
                            self.apu_reg[i % 0x18]
                        }
                    }
                    // 高三位为 3, [$6000, $8000): 存档 SRAM 区
                    3 => self.sram[i & 0x1fff],
                    _ => unreachable!("Out of memory range!"),
                }
            }
        }

        pub fn write_memeory(&mut self, addr: u16, val: u8) {
            let i: usize = addr.into();
            let j: usize = i >> 13;
            match j {
                0 => {
                    self.ram[i & 0x7ff] = val;
                }
                1 => {
                    self.ppu_reg.write(&mut self.ppu_mem, addr, val);
                }
                2 => {
                    if addr & 0x1f == 0x14 {
                        //DMA write
                        let addr_dma = (val as u16) << 8;
                        let offset: usize = (addr_dma & 0x1f00).into();
                        let src = match addr_dma >> 13 {
                            0 => {
                                let mirror = offset & 0x7ff;
                                &self.ram[mirror..mirror + 256]
                            }
                            3 => &self.sram[offset..offset + 256],
                            4 | 5 | 6 | 7 => &self.prg_rom[j - 4][offset..256],
                            1 | 2 => {
                                unimplemented!("Can't be operating by DMA.");
                            }
                            _ => unreachable!("Out of memory range!"),
                        };
                        if self.ppu_reg.oam_addr == 0 {
                            self.ppu_mem.oam.clone_from_slice(src);
                        } else {
                            let off = (255 - self.ppu_reg.oam_addr + 1) as usize;
                            self.ppu_mem.oam[self.ppu_reg.oam_addr as usize..=255]
                                .clone_from_slice(&src[..off]);
                            self.ppu_mem.oam[..self.ppu_reg.oam_addr as usize]
                                .clone_from_slice(&src[off..]);
                        }
                        cpu_cycles_add(513);
                        if get_cpu_cycles() & 0x1 == 0x1 {
                            cpu_cycles_add(1);
                        }
                    } else if addr & 0x1f == 0x16 {
                        if val & 0x1 == 0 {
                            self.key_indx[0] = 0;
                            self.key_indx[1] = 0;
                        }
                    } else {
                        self.apu_reg[i % 0x18] = val;
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
        pub fn push(&mut self, sp_piont: &mut u8, data: u8) {
            self.ram[0x100 + *sp_piont as usize] = data;
            *sp_piont = (*sp_piont).wrapping_sub(1);
        }
        pub fn pop(&self, sp_piont: &mut u8) -> u8 {
            *sp_piont = (*sp_piont).wrapping_add(1);
            self.ram[0x100 + *sp_piont as usize]
        }
    }

    use bitfield;
    use bitflags::bitflags;
    use std::fmt;

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
        //add for emulator
        pub irq_counter: u8,
        pub irq_flag: u8,
        pub irq_in_process: u8,
        pub nmi_in_process: u8,
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
                irq_counter: 0,
                irq_flag: 0,
                irq_in_process: 0,
                nmi_in_process: 0,
            }
        }

        pub fn reset(&mut self, mem: &mut MemMap) {
            let pcl = mem.read_memeory(REST_VECT_ADDR);
            let pch = mem.read_memeory(REST_VECT_ADDR + 1);

            *self = Register {
                pc: (pch as u16) << 8 | (pcl as u16),
                sp: 0xfd,
                p: { ProcessorStatus(0x34) },
                a: 0,
                x: 0,
                y: 0,
                irq_counter: 0,
                irq_flag: 0,
                irq_in_process: 0,
                nmi_in_process: 0,
            };
        }
    }

    pub fn nmi_handler(cpu_reg: &mut Register, mem: &mut MemMap) {
        let mut val_pc = cpu_reg.pc;
        mem.push(&mut cpu_reg.sp, (val_pc >> 8) as u8);
        mem.push(&mut cpu_reg.sp, (val_pc & 0xff) as u8);
        mem.push(
            &mut cpu_reg.sp,
            (cpu_reg.p.0 | ProcessorStatusFlags::FLAG_R.bits()) as u8,
        );
        cpu_reg.p.set_i(true);
        val_pc = mem.read_memeory(NMI_VECT_ADDR) as u16;
        val_pc |= (mem.read_memeory(NMI_VECT_ADDR + 1) as u16) << 8;
        cpu_reg.pc = val_pc;
        cpu_cycles_add(7);
    }

    #[derive(Debug)]
    pub enum AddressingMode {
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
        pub fn addressing(&self, cpu_reg: &mut Register, mem: &mut MemMap) -> u16 {
            match self {
                AddressingMode::Acc => 0,
                AddressingMode::Imp => 0,
                AddressingMode::Imm => {
                    let addr = cpu_reg.pc;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr
                }
                AddressingMode::Abs => {
                    let mut addr: u16 = mem.read_memeory(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr |= (mem.read_memeory(cpu_reg.pc) as u16) << 8;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr
                }
                AddressingMode::ZpAbs => {
                    let addr: u16 = mem.read_memeory(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr
                }
                AddressingMode::AbsXI => {
                    let mut addr: u16 = mem.read_memeory(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr |= (mem.read_memeory(cpu_reg.pc) as u16) << 8;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr.wrapping_add(cpu_reg.x as u16)
                }
                AddressingMode::AbsXI1C => {
                    let mut addr: u16 = mem.read_memeory(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr |= (mem.read_memeory(cpu_reg.pc) as u16) << 8;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    let val = addr.wrapping_add(cpu_reg.x as u16);
                    cpu_cycles_add((((addr ^ val) >> 8) & 1) as u32);
                    val
                }
                AddressingMode::AbsYI => {
                    let mut addr: u16 = mem.read_memeory(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr |= (mem.read_memeory(cpu_reg.pc) as u16) << 8;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr.wrapping_add(cpu_reg.y as u16)
                }
                AddressingMode::AbsYI1C => {
                    let mut addr: u16 = mem.read_memeory(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr |= (mem.read_memeory(cpu_reg.pc) as u16) << 8;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    let val = addr.wrapping_add(cpu_reg.y as u16);
                    cpu_cycles_add(((addr ^ val) >> 8) as u32 & 1);
                    val
                }
                AddressingMode::ZpXI => {
                    let addr: u16 = mem.read_memeory(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    (addr.wrapping_add(cpu_reg.x as u16)) & 0x00ff
                }
                AddressingMode::ZpYI => {
                    let addr: u16 = mem.read_memeory(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    (addr.wrapping_add(cpu_reg.y as u16)) & 0x00ff
                }
                AddressingMode::Ind => {
                    let mut addr1: u16 = mem.read_memeory(cpu_reg.pc) as u16;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    addr1 |= (mem.read_memeory(cpu_reg.pc) as u16) << 8;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    let addr2: u16 = (addr1 & 0xff00) | ((addr1.wrapping_add(1)) & 0x00ff); // 刻意实现 6502 的 BUG
                    (mem.read_memeory(addr1) as u16) | ((mem.read_memeory(addr2) as u16) << 8)
                }
                AddressingMode::IndX => {
                    let mut addr = mem.read_memeory(cpu_reg.pc);
                    addr = addr.wrapping_add(cpu_reg.x);
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    (mem.read_memeory(addr as u16) as u16)
                        | ((mem.read_memeory(addr.wrapping_add(1) as u16) as u16) << 8)
                }
                AddressingMode::IndY => {
                    let addr = mem.read_memeory(cpu_reg.pc);
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    let val = (mem.read_memeory(addr as u16) as u16)
                        | ((mem.read_memeory(addr.wrapping_add(1) as u16) as u16) << 8);
                    val.wrapping_add(cpu_reg.y as u16)
                }
                AddressingMode::IndY1C => {
                    let addr = mem.read_memeory(cpu_reg.pc);
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    let mut val = (mem.read_memeory(addr as u16) as u16)
                        | ((mem.read_memeory(addr.wrapping_add(1) as u16) as u16) << 8);
                    val = val.wrapping_add(cpu_reg.y as u16);
                    cpu_cycles_add((((addr as u16 ^ val) >> 8) & 1) as u32);
                    val
                }
                AddressingMode::Rel => {
                    let addr = mem.read_memeory(cpu_reg.pc) as i8;
                    cpu_reg.pc = cpu_reg.pc.wrapping_add(1);
                    cpu_reg.pc.saturating_add_signed(addr as i16)
                }
                AddressingMode::Unk => unimplemented!("Unimplemented addressing mode!"),
            }
        }
    }

    #[derive(Debug)]
    pub enum Instruction {
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
        pub fn exec(&self, cpu_reg: &mut Register, mem: &mut MemMap, addr: u16) {
            match self {
                Instruction::LDA => {
                    // LDA--由存储器取数送入累加器 A    M -> A
                    cpu_reg.a = mem.read_memeory(addr);
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::LDX => {
                    // LDX--由存储器取数送入寄存器 X    M -> X
                    cpu_reg.x = mem.read_memeory(addr);
                    cpu_reg.p.check_set_n(cpu_reg.x);
                    cpu_reg.p.check_set_z(cpu_reg.x);
                }
                Instruction::LDY => {
                    // LDY--由存储器取数送入寄存器 Y    M -> Y
                    cpu_reg.y = mem.read_memeory(addr);
                    cpu_reg.p.check_set_n(cpu_reg.y);
                    cpu_reg.p.check_set_z(cpu_reg.y);
                }
                Instruction::STA => {
                    // STA--将累加器 A 的数送入存储器    A -> M
                    mem.write_memeory(addr, cpu_reg.a);
                }
                Instruction::STX => {
                    // STX--将寄存器 X 的数送入存储器    X -> M
                    mem.write_memeory(addr, cpu_reg.x);
                }
                Instruction::STY => {
                    // STY--将寄存器 Y 的数送入存储器    Y -> M
                    mem.write_memeory(addr, cpu_reg.y);
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
                    let src = mem.read_memeory(addr);
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
                    let src = mem.read_memeory(addr);
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
                    let mut val = mem.read_memeory(addr);
                    val = val.wrapping_add(1);
                    mem.write_memeory(addr, val);
                    cpu_reg.p.check_set_n(val);
                    cpu_reg.p.check_set_z(val);
                }
                Instruction::DEC => {
                    // DEC--存储器单元内容减 1  M-1 -> M
                    let mut val = mem.read_memeory(addr);
                    val = val.wrapping_sub(1);
                    mem.write_memeory(addr, val);
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
                    cpu_reg.a &= mem.read_memeory(addr);
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::ORA => {
                    // ORA--存储器与累加器相或，结果送累加器  A|M -> A
                    cpu_reg.a |= mem.read_memeory(addr);
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::EOR => {
                    // EOR--存储器与累加器异或，结果送累加器  A≮M -> A
                    cpu_reg.a ^= mem.read_memeory(addr);
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
                }
                Instruction::SEI => {
                    // SEI--设置中断禁止 V         1 -> I
                    cpu_reg.p.set_i(true);
                }
                Instruction::CMP => {
                    // CMP--累加器和存储器比较
                    let reslt: u16 = (cpu_reg.a as i16 - mem.read_memeory(addr) as i16) as u16;
                    cpu_reg.p.set_c(reslt & 0x8000 == 0);
                    cpu_reg.p.check_set_n(reslt as u8);
                    cpu_reg.p.check_set_z(reslt as u8);
                }
                Instruction::CPX => {
                    // CPX--寄存器 X 的内容和存储器比较
                    let reslt: u16 = (cpu_reg.x as i16 - mem.read_memeory(addr) as i16) as u16;
                    cpu_reg.p.set_c(reslt < 256);
                    cpu_reg.p.check_set_n(reslt as u8);
                    cpu_reg.p.check_set_z(reslt as u8);
                }
                Instruction::CPY => {
                    // CPY--寄存器 Y 的内容和存储器比较
                    let reslt: u16 = (cpu_reg.y as i16 - mem.read_memeory(addr) as i16) as u16;
                    cpu_reg.p.set_c(reslt < 256);
                    cpu_reg.p.check_set_n(reslt as u8);
                    cpu_reg.p.check_set_z(reslt as u8);
                }
                Instruction::BIT => {
                    // BIT--位测试
                    let val = mem.read_memeory(addr);
                    cpu_reg.p.check_set_v(val & 0x40);
                    cpu_reg.p.check_set_n(val);
                    cpu_reg.p.check_set_z(cpu_reg.a & val);
                }
                Instruction::ASL => {
                    // ASL--算术左移 储存器
                    let mut val = mem.read_memeory(addr);
                    cpu_reg.p.check_set_c(val & 0x80);
                    val = val << 1;
                    mem.write_memeory(addr, val);
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
                    let mut val = mem.read_memeory(addr);
                    cpu_reg.p.check_set_c(val & 0x01);
                    val = val >> 1;
                    mem.write_memeory(addr, val);
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
                    let mut val_u16: u16 = mem.read_memeory(addr) as u16;
                    val_u16 <<= 1;
                    val_u16 |= if cpu_reg.p.c() { 1 } else { 0 };
                    cpu_reg.p.check_set_c((val_u16 >> 8) as u8);
                    let val_u8 = val_u16 as u8;
                    mem.write_memeory(addr, val_u8);
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
                    let mut val_u16: u16 = mem.read_memeory(addr) as u16;
                    val_u16 |= (if cpu_reg.p.c() { 1 } else { 0 }) << 8;
                    cpu_reg.p.check_set_c((val_u16 & 0x1) as u8);
                    let val_u8 = (val_u16 >> 1) as u8;
                    mem.write_memeory(addr, val_u8);
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
                    let pcl: u16 = mem.read_memeory(BRK_VECT_ADDR) as u16;
                    let pch: u16 = mem.read_memeory(BRK_VECT_ADDR + 1) as u16;
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
                    cpu_reg.irq_counter =
                        cpu_reg.irq_in_process & cpu_reg.irq_flag & ((!cpu_reg.p.i()) as u8);
                    cpu_reg.irq_in_process = 0;
                }
                //---  组合指令  ----------
                Instruction::ASR | Instruction::ALR => {
                    // [Unofficial&Combo] AND+LSR
                    //= SFC_INS_ALR{}// 有消息称是叫这个
                    cpu_reg.a &= mem.read_memeory(addr);
                    cpu_reg.p.check_set_c(cpu_reg.a & 0x1);
                    cpu_reg.a >>= 1;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::ANC | Instruction::AAC => {
                    // [Unofficial&Combo] AND+N2C?
                    cpu_reg.a &= mem.read_memeory(addr);
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                    cpu_reg.p.set_c(cpu_reg.p.n());
                }
                Instruction::ARR => {
                    // [Unofficial&Combo] AND+ROR [类似]
                    cpu_reg.a &= mem.read_memeory(addr);
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
                    let val = (cpu_reg.a & cpu_reg.x) as i16 - mem.read_memeory(addr) as i16;
                    cpu_reg.x = (val & 0xff) as u8;
                    cpu_reg.p.check_set_n(cpu_reg.x);
                    cpu_reg.p.check_set_z(cpu_reg.x);
                    cpu_reg.p.set_c(val >= 0);
                }
                Instruction::LAX => {
                    // [Unofficial&Combo] LDA+TAX
                    cpu_reg.a = mem.read_memeory(addr);
                    cpu_reg.x = cpu_reg.a;
                    cpu_reg.p.check_set_n(cpu_reg.x);
                    cpu_reg.p.check_set_z(cpu_reg.x);
                }
                Instruction::SAX => {
                    // [Unofficial&Combo] STA&STX [类似]
                    mem.write_memeory(addr, cpu_reg.a & cpu_reg.x);
                }
                //--- 读改写指令 ----------
                Instruction::DCP => {
                    // [Unofficial& RMW ] DEC+CMP
                    let mut val = mem.read_memeory(addr);
                    val = val.wrapping_sub(1);
                    mem.write_memeory(addr, val);

                    let val_i16 = cpu_reg.a as i16 - val as i16;
                    cpu_reg.p.set_c(val_i16 >= 0);
                    cpu_reg.p.check_set_n((val_i16 & 0xff) as u8);
                    cpu_reg.p.check_set_z((val_i16 & 0xff) as u8);
                }
                Instruction::ISC | Instruction::ISB => {
                    // [Unofficial& RMW ] INC+SBC
                    //INC
                    let mut val = mem.read_memeory(addr);
                    val = val.wrapping_add(1);
                    mem.write_memeory(addr, val);
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
                    let mut val_u16: u16 = mem.read_memeory(addr) as u16;
                    val_u16 <<= 1;
                    if cpu_reg.p.c() {
                        val_u16 |= 1;
                    }
                    cpu_reg.p.set_c(val_u16 > 0xff);
                    let val_u8: u8 = val_u16 as u8;
                    mem.write_memeory(addr, val_u8);
                    // AND
                    cpu_reg.a &= val_u8;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::RRA => {
                    // [Unofficial& RMW ] ROR+AND
                    let mut val_u16: u16 = mem.read_memeory(addr) as u16;

                    if cpu_reg.p.c() {
                        val_u16 |= 0x100;
                    }
                    cpu_reg.p.check_set_c((val_u16 & 0x1) as u8);
                    val_u16 >>= 1;

                    //ADC
                    let src = val_u16 as u8;
                    mem.write_memeory(addr, src);
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
                    let mut val = mem.read_memeory(addr);
                    cpu_reg.p.check_set_c(val & 0x80);
                    val = val << 1;
                    mem.write_memeory(addr, val);
                    // ORA
                    cpu_reg.a |= val;
                    cpu_reg.p.check_set_n(cpu_reg.a);
                    cpu_reg.p.check_set_z(cpu_reg.a);
                }
                Instruction::SRE => {
                    // LSR
                    let mut val = mem.read_memeory(addr);
                    cpu_reg.p.check_set_c(val & 0x1);
                    val = val >> 1;
                    mem.write_memeory(addr, val);
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
            cpu_cycles_add((((addr ^ cpu_reg.pc) >> 8) & 1) as u32 + 1);
            cpu_reg.pc = addr;
        }
    }

    #[derive(Debug)]
    pub struct InstruAddring<'a> {
        pub instru_name: &'a str,
        pub instru: Instruction,
        pub instru_base_cycle: u8,
        pub addring_mode: AddressingMode,
    }

    #[rustfmt::skip]
    pub const ISTRU_OP_CODE: [InstruAddring; 256] = [
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

    pub fn execute_one_instruction(cpu_reg: &mut Register, mem: &mut MemMap) {
        let machine_code: u8;

        machine_code = mem.read_memeory(cpu_reg.pc);
        // print!(
        //     "code addr {:#x}, machine_code {:#x}, ",
        //     cpu_reg.pc, machine_code
        // );
        cpu_reg.pc = cpu_reg.pc.wrapping_add(1);

        let instru_addring = &ISTRU_OP_CODE[machine_code as usize];
        cpu_cycles_add(instru_addring.instru_base_cycle as u32);

        let op_addr = instru_addring.addring_mode.addressing(cpu_reg, mem);
        // print!(
        //     "asm instruction {}, operation addr {:#x}, ",
        //     instru_addring.instru_name, op_addr
        // );

        instru_addring.instru.exec(cpu_reg, mem, op_addr);

        // println!(
        //     "a {:#x}, x {:#x}, y {:#x}, p {:#x}, sp {:#x}, cycles {}",
        //     cpu_reg.a, cpu_reg.x, cpu_reg.y, cpu_reg.p.0, cpu_reg.sp, get_cpu_cycles()
        // );
        // println!("{:#?}", instru_addring);
    }
}

pub mod disassembly {}
