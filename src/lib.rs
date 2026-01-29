use std::fmt;

const UNINITIALIZED_REGISTER_PATTERN: u16 = 0x0000;
fn unwrap_register(reg: Option<u16>) -> u16 {
    match reg {
        Some(val) => val,
        None => UNINITIALIZED_REGISTER_PATTERN,
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DCoreInstruction {
    // Debug Operations
    /// .prdez RX
    PrDez(u16),
    /// .prnewline
    PrNewLine,
    /// .prstr RX
    PrStr(u16),

    // ALU Operations
    /// mov RY, RX
    Mov(u16, u16),
    /// addu RY, RX
    AddU(u16, u16),
    /// addc RY, RX
    AddC(u16, u16),
    /// subu RY, RX
    SubU(u16, u16),
    /// and RY, RX
    And(u16, u16),
    /// or RY, RX
    Or(u16, u16),
    /// xor RY, RX
    Xor(u16, u16),
    /// not RX
    Not(u16),

    // Shift Operations
    /// lsl RX, RY
    Lsl(u16, u16),
    /// lsr RX, RY
    Lsr(u16, u16),
    /// asr RX, RY
    Asr(u16, u16),
    /// lslc RX
    LslC(u16),
    /// lsrc RX
    LsrC(u16),
    /// asrc RX
    AsrC(u16),

    // Compare Operations
    /// cmpe RX, RY
    CmpE(u16, u16),
    /// cmpne RX, RY
    CmpNE(u16, u16),
    /// cmpgt RX, RY
    CmpGT(u16, u16),
    /// cmplt RX, RY
    CmpLT(u16, u16),

    // Immediate Operations
    /// movi RX, IMM4
    MovI(u16, u16),
    /// addi RX, IMM4
    AddI(u16, u16),
    /// subi RX, IMM4
    SubI(u16, u16),
    /// andi RX, IMM4
    AndI(u16, u16),
    /// lsli RX, IMM4
    LslI(u16, u16),
    /// lsri RX, IMM4
    LsrI(u16, u16),
    /// bseti RX, IMM4
    BSetI(u16, u16),
    /// bclri RX, IMM4
    BClrI(u16, u16),

    // Memory Operations
    /// ldw RX, IMM4(RY)
    LdW(u16, u16, u16),
    /// stw RX, IMM4(RY)
    StW(u16, u16, u16),

    // Control Flow Operations
    /// br IMM12
    Br(i16),
    /// jsr IMM12
    Jsr(i16),
    /// bt IMM12
    Bt(i16),
    /// bf IMM12
    Bf(i16),
    /// jmp RX
    Jmp(u16),
    /// halt
    Halt,
}

impl fmt::Display for DCoreInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // Debug Operations
            Self::PrDez(rx) => write!(f, ".prdez R{}", rx),
            Self::PrNewLine => write!(f, ".prnewline"),
            Self::PrStr(rx) => write!(f, ".prstr R{}", rx),

            // ALU Operations (Two Registers)
            Self::Mov(rx, ry) => write!(f, "mov R{}, R{}", rx, ry),
            Self::AddU(rx, ry) => write!(f, "addu R{}, R{}", rx, ry),
            Self::AddC(rx, ry) => write!(f, "addc R{}, R{}", rx, ry),
            Self::SubU(rx, ry) => write!(f, "subu R{}, R{}", rx, ry),
            Self::And(rx, ry) => write!(f, "and R{}, R{}", rx, ry),
            Self::Or(rx, ry) => write!(f, "or R{}, R{}", rx, ry),
            Self::Xor(rx, ry) => write!(f, "xor R{}, R{}", rx, ry),

            // ALU Operations (One Register)
            Self::Not(rx) => write!(f, "not R{}", rx),

            // Shift Operations (Two Registers)
            Self::Lsl(rx, ry) => write!(f, "lsl R{}, R{}", rx, ry),
            Self::Lsr(rx, ry) => write!(f, "lsr R{}, R{}", rx, ry),
            Self::Asr(rx, ry) => write!(f, "asr R{}, R{}", rx, ry),

            // Shift Operations (One Register / Carry)
            Self::LslC(rx) => write!(f, "lslc R{}", rx),
            Self::LsrC(rx) => write!(f, "lsrc R{}", rx),
            Self::AsrC(rx) => write!(f, "asrc R{}", rx),

            // Compare Operations
            Self::CmpE(rx, ry) => write!(f, "cmpe R{}, R{}", rx, ry),
            Self::CmpNE(rx, ry) => write!(f, "cmpne R{}, R{}", rx, ry),
            Self::CmpGT(rx, ry) => write!(f, "cmpgt R{}, R{}", rx, ry),
            Self::CmpLT(rx, ry) => write!(f, "cmplt R{}, R{}", rx, ry),

            // Immediate Operations
            Self::MovI(rx, imm) => write!(f, "movi R{}, {}", rx, imm),
            Self::AddI(rx, imm) => write!(f, "addi R{}, {}", rx, imm),
            Self::SubI(rx, imm) => write!(f, "subi R{}, {}", rx, imm),
            Self::AndI(rx, imm) => write!(f, "andi R{}, {}", rx, imm),
            Self::LslI(rx, imm) => write!(f, "lsli R{}, {}", rx, imm),
            Self::LsrI(rx, imm) => write!(f, "lsri R{}, {}", rx, imm),
            Self::BSetI(rx, imm) => write!(f, "bseti R{}, {}", rx, imm),
            Self::BClrI(rx, imm) => write!(f, "bclri R{}, {}", rx, imm),

            // Memory Operations
            // Format: ldw Dest, Offset(Base)
            Self::LdW(rx, ry, imm) => write!(f, "ldw R{}, {}(R{})", rx, imm, ry),
            Self::StW(rx, ry, imm) => write!(f, "stw R{}, {}(R{})", rx, imm, ry),

            // Control Flow Operations
            Self::Br(offset) => write!(f, "br {}", offset),
            Self::Jsr(offset) => write!(f, "jsr {}", offset),
            Self::Bt(offset) => write!(f, "bt {}", offset),
            Self::Bf(offset) => write!(f, "bf {}", offset),
            Self::Jmp(rx) => write!(f, "jmp R{}", rx),
            Self::Halt => write!(f, "halt"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StepDebugInfo {
    pub pc: u16,
    pub registers: [Option<u16>; 16],
    pub carry: bool,
}

impl StepDebugInfo {
    fn new(core: &DCoreCPU) -> Self {
        Self {
            pc: core.pc,
            registers: core.registers,
            carry: core.carry,
        }
    }
}

#[derive(Debug, Clone)]
pub struct DCoreCPU {
    /// R0-R15 Registers of D-Core CPU
    /// These start out uninitialized to 0x0000
    pub registers: [Option<u16>; 16],
    pub pc: u16,
    pub carry: bool,
    pub mmu: DCoreMMU,
    pub halted: bool,
}

impl Default for DCoreCPU {
    fn default() -> Self {
        Self::new()
    }
}

impl DCoreCPU {
    pub fn new() -> Self {
        Self {
            // 0xf0f0 is an abitrary value chosen for unintialized registers
            registers: [None; 16],
            pc: 0,
            carry: false,
            halted: false,
            mmu: DCoreMMU {
                rom_chip: [None; 32768],
                ram_chip: [None; 32768],
            },
        }
    }

    pub fn step(&mut self) -> (StepDebugInfo, Option<DCoreInstruction>, StepDebugInfo) {
        let before_step_debug_info = StepDebugInfo::new(self);
        // Fetch
        let instruction = unwrap_memory(self.mmu.read_halfword(self.pc));

        // Advance to next instruction
        self.pc += 2;

        // Decode
        let opcode = (0xf000 & instruction) >> 12;
        let x_register = 0x000f & instruction;
        let y_register = (0x00f0 & instruction) >> 4;
        let alu_opc = (0x1f00 & instruction) >> 8;
        let stw_xa = (0x0f00 & instruction) >> 8;
        let imm_12 = 0x0fff & instruction;
        let imm_4 = (0x00f0 & instruction) >> 4;

        // Execute
        let decoded_instruction = match opcode {
            0b0000 => self.debug_operation(instruction),
            // ALU Operations
            0b0010 | 0b0011 => self.alu_operation(alu_opc, x_register, y_register, imm_4),
            // ldw
            0b0100 => {
                let base_addr = unwrap_register(self.registers[y_register as usize]);
                let addr = base_addr.wrapping_add(stw_xa << 1);

                self.registers[x_register as usize] =
                    Some(unwrap_memory(self.mmu.read_halfword(addr)));

                Some(DCoreInstruction::LdW(x_register, y_register, stw_xa))
            }
            // stw
            0b0101 => {
                let base_addr = unwrap_register(self.registers[y_register as usize]);
                let addr = base_addr.wrapping_add(stw_xa << 1);
                self.mmu
                    .write_halfword(addr, unwrap_register(self.registers[x_register as usize]));

                Some(DCoreInstruction::StW(x_register, y_register, stw_xa))
            }
            // br
            0b1000 => {
                let pc_offset = self.br_pc_to_imm12(imm_12);
                Some(DCoreInstruction::Br(pc_offset))
            }
            // jsr
            0b1001 => {
                self.registers[15] = Some(self.pc);
                let pc_offset = self.br_pc_to_imm12(imm_12);
                Some(DCoreInstruction::Jsr(pc_offset))
            }
            // bt
            0b1010 => {
                if self.carry {
                    let _ = self.br_pc_to_imm12(imm_12);
                }
                let pc_offset = Self::calculate_signed_imm12(imm_12);
                Some(DCoreInstruction::Bt(pc_offset))
            }
            // bf
            0b1011 => {
                if !self.carry {
                    let _ = self.br_pc_to_imm12(imm_12);
                }
                let pc_offset = Self::calculate_signed_imm12(imm_12);
                Some(DCoreInstruction::Bf(pc_offset))
            }
            // jmp
            0b1100 => {
                self.pc = unwrap_register(self.registers[x_register as usize]);
                Some(DCoreInstruction::Jmp(x_register))
            }
            // halt
            0b1111 => {
                self.pc -= 2;
                self.halted = true;
                Some(DCoreInstruction::Halt)
            }
            _ => {
                println!("Unknown opcode {:04x} at PC {:04x}", opcode, self.pc);
                println!("Instruction was {:04x}", instruction);
                None
            }
        };

        let after_step_debug_info = StepDebugInfo::new(self);

        (
            before_step_debug_info,
            decoded_instruction,
            after_step_debug_info,
        )
    }

    fn calculate_signed_imm12(imm12: u16) -> i16 {
        u16::cast_signed(imm12 << 4) >> 4
    }

    /// PC = PC + 2 + imm12 (signed)
    fn br_pc_to_imm12(&mut self, _imm12: u16) -> i16 {
        let imm_12_i16 = Self::calculate_signed_imm12(_imm12);

        if imm_12_i16 > 0 {
            self.pc = self.pc.wrapping_add(_imm12);
        } else {
            self.pc = self.pc.wrapping_sub(imm_12_i16.unsigned_abs());
        }

        imm_12_i16
    }

    fn alu_operation(
        &mut self,
        alu_opc: u16,
        x_reg: u16,
        y_reg: u16,
        imm4: u16,
    ) -> Option<DCoreInstruction> {
        match alu_opc {
            // mov
            0b0_0000 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[y_reg as usize]));
                Some(DCoreInstruction::Mov(x_reg, y_reg))
            }
            // addu
            0b0_0001 => {
                self.registers[x_reg as usize] = Some(
                    unwrap_register(self.registers[x_reg as usize])
                        .wrapping_add(unwrap_register(self.registers[y_reg as usize])),
                );
                Some(DCoreInstruction::AddU(x_reg, y_reg))
            }
            // addc
            0b0_0010 => {
                let (sum1, overflow1) = unwrap_register(self.registers[x_reg as usize])
                    .overflowing_add(unwrap_register(self.registers[y_reg as usize]));

                let (output_sum, overflow2) = sum1.overflowing_add(if self.carry { 1 } else { 0 });

                self.registers[x_reg as usize] = Some(output_sum);
                self.carry = overflow1 || overflow2;
                Some(DCoreInstruction::AddC(x_reg, y_reg))
            }
            // subu
            0b0_0011 => {
                self.registers[x_reg as usize] = Some(
                    unwrap_register(self.registers[x_reg as usize])
                        .wrapping_sub(unwrap_register(self.registers[y_reg as usize])),
                );
                Some(DCoreInstruction::SubU(x_reg, y_reg))
            }
            // and
            0b0_0100 => {
                let mut x_val = unwrap_register(self.registers[x_reg as usize]);
                x_val &= unwrap_register(self.registers[y_reg as usize]);
                self.registers[x_reg as usize] = Some(x_val);
                Some(DCoreInstruction::And(x_reg, y_reg))
            }
            // or
            0b0_0101 => {
                // self.registers[x_reg as usize] |= self.registers[y_reg as usize];
                let mut x_val = unwrap_register(self.registers[x_reg as usize]);
                x_val |= unwrap_register(self.registers[y_reg as usize]);
                self.registers[x_reg as usize] = Some(x_val);
                Some(DCoreInstruction::Or(x_reg, y_reg))
            }
            // xor
            0b0_0110 => {
                let mut x_val = unwrap_register(self.registers[x_reg as usize]);
                x_val ^= unwrap_register(self.registers[y_reg as usize]);
                self.registers[x_reg as usize] = Some(x_val);
                Some(DCoreInstruction::Xor(x_reg, y_reg))
            }
            // not
            0b0_0111 => {
                self.registers[x_reg as usize] =
                    Some(!unwrap_register(self.registers[y_reg as usize]));
                Some(DCoreInstruction::Not(x_reg))
            }

            // lsl
            0b0_1000 => {
                let shift_amount = unwrap_register(self.registers[y_reg as usize]) & 0xf;
                let x_reg = unwrap_register(self.registers[x_reg as usize]);
                self.registers[x_reg as usize] = Some(x_reg << shift_amount);
                Some(DCoreInstruction::Lsl(x_reg, y_reg))
            }
            // lsr
            0b0_1001 => {
                let shift_amount = unwrap_register(self.registers[y_reg as usize]) & 0xf;
                let x_reg = unwrap_register(self.registers[x_reg as usize]);
                self.registers[x_reg as usize] = Some(x_reg >> shift_amount);
                Some(DCoreInstruction::Lsr(x_reg, y_reg))
            }
            // asr
            0b0_1010 => {
                let shift_amount = unwrap_register(self.registers[y_reg as usize]) & 0xf;
                let val = unwrap_register(self.registers[x_reg as usize]) as i16;
                self.registers[x_reg as usize] = Some((val >> shift_amount) as u16);
                Some(DCoreInstruction::Asr(x_reg, y_reg))
            }
            // lslc
            0b0_1100 => {
                self.carry = (unwrap_register(self.registers[x_reg as usize]) & 0x8000) != 0;
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]) << 1);
                Some(DCoreInstruction::LslC(x_reg))
            }
            // lsrc
            0b0_1101 => {
                self.carry = (unwrap_register(self.registers[x_reg as usize]) & 0x1) != 0;
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]) >> 1);
                Some(DCoreInstruction::LsrC(x_reg))
            }
            // asrc
            0b0_1110 => {
                self.carry = (unwrap_register(self.registers[x_reg as usize]) & 0x1) != 0;
                self.registers[x_reg as usize] =
                    Some(((unwrap_register(self.registers[x_reg as usize]) as i16) >> 1) as u16);
                Some(DCoreInstruction::AsrC(x_reg))
            }

            // cmpe
            0b1_0000 => {
                self.carry = self.registers[x_reg as usize] == self.registers[y_reg as usize];
                Some(DCoreInstruction::CmpE(x_reg, y_reg))
            }
            // cmpne
            0b1_0001 => {
                self.carry = self.registers[x_reg as usize] != self.registers[y_reg as usize];
                Some(DCoreInstruction::CmpNE(x_reg, y_reg))
            }
            // cmpgt
            0b1_0010 => {
                self.carry = self.registers[x_reg as usize] > self.registers[y_reg as usize];
                Some(DCoreInstruction::CmpGT(x_reg, y_reg))
            }
            // cmplt
            0b1_0011 => {
                self.carry = self.registers[x_reg as usize] < self.registers[y_reg as usize];
                Some(DCoreInstruction::CmpLT(x_reg, y_reg))
            }

            // movi
            0b1_0100 => {
                self.registers[x_reg as usize] = Some(imm4);
                Some(DCoreInstruction::MovI(x_reg, imm4))
            }
            // addi
            0b1_0101 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]).wrapping_add(imm4));
                Some(DCoreInstruction::AddI(x_reg, imm4))
            }
            // subi
            0b1_0110 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]).wrapping_sub(imm4));
                Some(DCoreInstruction::SubI(x_reg, imm4))
            }
            // andi
            0b1_0111 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]) & imm4);
                Some(DCoreInstruction::AndI(x_reg, imm4))
            }
            // lsli
            0b1_1000 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]) << imm4);
                Some(DCoreInstruction::LslI(x_reg, imm4))
            }
            // lsri
            0b1_1001 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]) >> imm4);
                Some(DCoreInstruction::LsrI(x_reg, imm4))
            }
            // bseti
            0b1_1010 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]) | (1 << imm4));
                Some(DCoreInstruction::BSetI(x_reg, imm4))
            }
            // bclri
            0b1_1011 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]) & !(1 << imm4));
                Some(DCoreInstruction::BClrI(x_reg, imm4))
            }
            _ => {
                println!("Unknown ALU opcode {:04x}", alu_opc);
                None
            }
        }
    }

    fn debug_operation(&self, instruction: u16) -> Option<DCoreInstruction> {
        if (instruction >> 8) != 0x8 {
            return None;
        }
        let x_register = 0x000f & instruction;
        let dbg_opcode = (0x00f0 & instruction) >> 4;

        match dbg_opcode {
            0b0000 => {
                // .prdez RX
                print!("{}", unwrap_register(self.registers[x_register as usize]));
                Some(DCoreInstruction::PrDez(x_register))
            }
            0b0011 => {
                // .prnewline
                println!();
                Some(DCoreInstruction::PrNewLine)
            }
            0b0010 => {
                // .prstr RX
                let start_addr = unwrap_register(self.registers[x_register as usize]);
                let mut current_addr = start_addr;
                loop {
                    let halfword = unwrap_memory(self.mmu.read_halfword(current_addr));
                    let low_byte = (halfword & 0x00FF) as u8;
                    let high_byte = (halfword >> 8) as u8;

                    if low_byte == 0 {
                        break;
                    }
                    print!("{}", low_byte as char);

                    if high_byte == 0 {
                        break;
                    }
                    print!("{}", high_byte as char);

                    current_addr = current_addr.wrapping_add(2);
                }
                Some(DCoreInstruction::PrStr(x_register))
            }
            _ => {
                println!("Unknown debug opcode {:04x}", instruction);
                None
            }
        }
    }
}

const UNINITIALIZED_MEMORY_PATTERN: u16 = 0x0000;
fn unwrap_memory(mem: Option<u16>) -> u16 {
    match mem {
        Some(val) => val,
        None => UNINITIALIZED_MEMORY_PATTERN,
    }
}

#[derive(Debug, Clone)]
pub struct DCoreMMU {
    pub rom_chip: [Option<u8>; 32768],
    pub ram_chip: [Option<u8>; 32768],
}

impl DCoreMMU {
    pub fn write_halfword(&mut self, addr: u16, value: u16) {
        if (0x7FFF..0x8000).contains(&addr) || addr == 0xFFFF {
            return;
        }

        if addr < 0x8000 {
            println!("Attempted write to ROM at address {:04x}", addr);
        } else {
            let ram_addr = (addr - 0x8000) as usize;
            self.ram_chip[ram_addr] = Some((value & 0xFF) as u8);
            self.ram_chip[ram_addr + 1] = Some((value >> 8) as u8);
        }
    }

    pub fn read_halfword(&self, addr: u16) -> Option<u16> {
        let chip = if addr < 0x8000 {
            &self.rom_chip
        } else {
            &self.ram_chip
        };
        let offset = if addr < 0x8000 {
            addr as usize
        } else {
            (addr - 0x8000) as usize
        };

        if offset >= 32767 {
            return None;
        }

        match (chip[offset], chip[offset + 1]) {
            (Some(l), Some(u)) => Some(((u as u16) << 8) | (l as u16)),
            _ => None,
        }
    }

    pub fn setup_write_halfword(&mut self, _addr: u16, _value: u16) {
        let (chip, offset) = if _addr < 0x8000 {
            (&mut self.rom_chip, _addr as usize)
        } else {
            (&mut self.ram_chip, (_addr - 0x8000) as usize)
        };

        if offset < 32767 {
            chip[offset] = Some((_value & 0xFF) as u8);
            chip[offset + 1] = Some(((_value & 0xFF00) >> 8) as u8);
        }
    }

    pub fn setup_read_halfword(&mut self, _addr: u16) -> Option<u16> {
        self.read_halfword(_addr)
    }

    pub fn dump_memory(&self, start_addr: u16, end_addr: u16) -> Vec<u16> {
        let mut memory_dump = Vec::new();
        for addr in (start_addr..end_addr).step_by(2) {
            memory_dump.push(self.read_halfword(addr).unwrap_or(0));
        }
        memory_dump
    }

    pub fn write_string(&mut self, start_addr: u16, content: &str) {
        // 1. Convert string to bytes and add the null terminator
        let mut bytes: Vec<u8> = content.bytes().collect();
        bytes.push(0x00); // The '\0' terminator

        // 2. Iterate over the bytes in chunks of 2
        for (i, chunk) in bytes.chunks(2).enumerate() {
            // Calculate the current address (2 bytes per halfword)
            let current_addr = start_addr + (i * 2) as u16;

            // Get the lower byte (first char)
            let low = chunk[0] as u16;

            // Get the upper byte (second char).
            // If the string length is odd (including null), pad high byte with 0.
            let high = if chunk.len() > 1 { chunk[1] as u16 } else { 0 };

            // 3. Pack them into a 16-bit halfword (Little Endian)
            // Format: [High Byte][Low Byte] -> 0xHHL L
            let value = (high << 8) | low;

            // 4. Write to MMU
            self.setup_write_halfword(current_addr, value);
        }
    }
}
