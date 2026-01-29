pub trait MMU {
    fn write_halfword(&mut self, addr: u16, value: u16);
    fn read_halfword(&self, addr: u16) -> Option<u16>;

    /// Intended for loading initial program/data into memory before execution
    /// Skips any protections (e.g. writing to ROM)
    /// and doesn't have side effects
    fn setup_write_halfword(&mut self, _addr: u16, _value: u16);

    /// Intended for loading initial program/data into memory before execution
    /// Skips any protections
    /// and doesn't have side effects
    fn setup_read_halfword(&mut self, _addr: u16) -> Option<u16>;

    fn dump_memory(&self, start_addr: u16, end_addr: u16) -> Vec<u16> {
        let mut memory_dump = Vec::new();
        for addr in (start_addr..end_addr).step_by(2) {
            memory_dump.push(self.read_halfword(addr).unwrap_or(0));
        }
        memory_dump
    }
}

const UNINITIALIZED_REGISTER_PATTERN: u16 = 0x0000;
fn unwrap_register(reg: Option<u16>) -> u16 {
    match reg {
        Some(val) => val,
        None => UNINITIALIZED_REGISTER_PATTERN,
    }
}

pub struct DCoreCPU {
    /// R0-R15 Registers of D-Core CPU
    /// These start out uninitialized to 0xf0f0
    pub registers: [Option<u16>; 16],
    pub pc: u16,
    pub carry: bool,
    pub mmu: Box<dyn MMU>,
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
            mmu: Box::new(DCoreMinimalMMU {
                rom_chip: [None; 32768],
                ram_chip: [None; 32768],
            }),
        }
    }

    pub fn step(&mut self) {
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
        match opcode {
            // ALU Operations
            0b0010 | 0b0011 => self.alu_operation(alu_opc, x_register, y_register, imm_4),
            // ldw
            0b0100 => {
                let base_addr = unwrap_register(self.registers[y_register as usize]);
                let addr = base_addr.wrapping_add(stw_xa << 1);

                self.registers[x_register as usize] =
                    Some(unwrap_memory(self.mmu.read_halfword(addr)));
                println!("ldw R{}, {}(R{})", x_register, stw_xa, y_register);
            }
            // stw
            0b0101 => {
                let base_addr = unwrap_register(self.registers[y_register as usize]);
                let addr = base_addr.wrapping_add(stw_xa << 1);
                self.mmu
                    .write_halfword(addr, unwrap_register(self.registers[x_register as usize]));
                println!("stw R{}, {}(R{})", x_register, stw_xa, y_register);
            }
            // br
            0b1000 => {
                let pc_offset = self.br_pc_to_imm12(imm_12);
                println!("br {}", (pc_offset));
            }
            // jsr
            0b1001 => {
                self.registers[15] = Some(self.pc);
                let pc_offset = self.br_pc_to_imm12(imm_12);
                println!("jsr {}", pc_offset);
            }
            // bt
            0b1010 => {
                if self.carry {
                    let _ = self.br_pc_to_imm12(imm_12);
                }
                println!("bt {}", Self::calculate_signed_imm12(imm_12));
            }
            // bf
            0b1011 => {
                if !self.carry {
                    let _ = self.br_pc_to_imm12(imm_12);
                }
                println!("bf {}", Self::calculate_signed_imm12(imm_12));
            }
            // jmp
            0b1100 => {
                self.pc = unwrap_register(self.registers[x_register as usize]);
                println!("jmp R{}", x_register);
            }
            // halt
            0b1111 => {
                self.pc -= 2;
                self.halted = true;
                println!("halt");
            }
            _ => {
                println!("Unknown opcode {:04x} at PC {:04x}", opcode, self.pc);
                println!("Instruction was {:04x}", instruction);
            }
        }
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

    fn alu_operation(&mut self, alu_opc: u16, x_reg: u16, y_reg: u16, imm4: u16) {
        match alu_opc {
            // mov
            0b0_0000 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[y_reg as usize]));
                println!("mov R{}, R{}", x_reg, y_reg);
            }
            // addu
            0b0_0001 => {
                self.registers[x_reg as usize] = Some(
                    unwrap_register(self.registers[x_reg as usize])
                        .wrapping_add(unwrap_register(self.registers[y_reg as usize])),
                );
                println!("addu R{}, R{}", x_reg, y_reg);
            }
            // addc
            0b0_0010 => {
                let (sum1, overflow1) = unwrap_register(self.registers[x_reg as usize])
                    .overflowing_add(unwrap_register(self.registers[y_reg as usize]));

                let (output_sum, overflow2) = sum1.overflowing_add(if self.carry { 1 } else { 0 });

                self.registers[x_reg as usize] = Some(output_sum);
                self.carry = overflow1 || overflow2;
                println!("addc R{}, R{}", x_reg, y_reg);
            }
            // subu
            0b0_0011 => {
                self.registers[x_reg as usize] = Some(
                    unwrap_register(self.registers[x_reg as usize])
                        .wrapping_sub(unwrap_register(self.registers[y_reg as usize])),
                );
                println!("subu R{}, R{}", x_reg, y_reg);
            }
            // and
            0b0_0100 => {
                let mut x_val = unwrap_register(self.registers[x_reg as usize]);
                x_val &= unwrap_register(self.registers[y_reg as usize]);
                self.registers[x_reg as usize] = Some(x_val);
                println!("and R{}, R{}", x_reg, y_reg);
            }
            // or
            0b0_0101 => {
                // self.registers[x_reg as usize] |= self.registers[y_reg as usize];
                let mut x_val = unwrap_register(self.registers[x_reg as usize]);
                x_val |= unwrap_register(self.registers[y_reg as usize]);
                self.registers[x_reg as usize] = Some(x_val);
                println!("or R{}, R{}", x_reg, y_reg);
            }
            // xor
            0b0_0110 => {
                let mut x_val = unwrap_register(self.registers[x_reg as usize]);
                x_val ^= unwrap_register(self.registers[y_reg as usize]);
                self.registers[x_reg as usize] = Some(x_val);
                println!("xor R{}, R{}", x_reg, y_reg);
            }
            // not
            0b0_0111 => {
                self.registers[x_reg as usize] =
                    Some(!unwrap_register(self.registers[y_reg as usize]));
                println!("not R{}, R{}", x_reg, y_reg);
            }

            // lsl
            0b0_1000 => {
                let shift_amount = unwrap_register(self.registers[y_reg as usize]) & 0xf;
                let x_reg = unwrap_register(self.registers[x_reg as usize]);
                self.registers[x_reg as usize] = Some(x_reg << shift_amount);
                println!("lsl R{}, R{}", x_reg, y_reg);
            }
            // lsr
            0b0_1001 => {
                let shift_amount = unwrap_register(self.registers[y_reg as usize]) & 0xf;
                let x_reg = unwrap_register(self.registers[x_reg as usize]);
                self.registers[x_reg as usize] = Some(x_reg >> shift_amount);
                println!("lsr R{}, R{}", x_reg, y_reg);
            }
            // asr
            0b0_1010 => {
                let shift_amount = unwrap_register(self.registers[y_reg as usize]) & 0xf;
                let val = unwrap_register(self.registers[x_reg as usize]) as i16;
                self.registers[x_reg as usize] = Some((val >> shift_amount) as u16);
                println!("asr R{}, R{}", x_reg, y_reg);
            }
            // lslc
            0b0_1100 => {
                self.carry = (unwrap_register(self.registers[x_reg as usize]) & 0x8000) != 0;
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]) << 1);
                println!("lslc R{}", x_reg);
            }
            // lsrc
            0b0_1101 => {
                self.carry = (unwrap_register(self.registers[x_reg as usize]) & 0x1) != 0;
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]) >> 1);
                println!("lsrc R{}", x_reg);
            }
            // asrc
            0b0_1110 => {
                self.carry = (unwrap_register(self.registers[x_reg as usize]) & 0x1) != 0;
                self.registers[x_reg as usize] =
                    Some(((unwrap_register(self.registers[x_reg as usize]) as i16) >> 1) as u16);
                println!("asrc R{}", x_reg);
            }

            // cmpe
            0b1_0000 => {
                self.carry = self.registers[x_reg as usize] == self.registers[y_reg as usize];
                println!("cmpe R{}, R{}", x_reg, y_reg);
            }
            // cmpne
            0b1_0001 => {
                self.carry = self.registers[x_reg as usize] != self.registers[y_reg as usize];
                println!("cmpne R{}, R{}", x_reg, y_reg);
            }
            // cmpgt
            0b1_0010 => {
                self.carry = self.registers[x_reg as usize] > self.registers[y_reg as usize];
                println!("cmpgt R{}, R{}", x_reg, y_reg);
            }
            // cmplt
            0b1_0011 => {
                self.carry = self.registers[x_reg as usize] < self.registers[y_reg as usize];
                println!("cmplt R{}, R{}", x_reg, y_reg);
            }

            // movi
            0b1_0100 => {
                self.registers[x_reg as usize] = Some(imm4);
                println!("movi R{}, {}", x_reg, imm4);
            }
            // addi
            0b1_0101 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]).wrapping_add(imm4));
                println!("addi R{}, {}", x_reg, imm4);
            }
            // subi
            0b1_0110 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]).wrapping_sub(imm4));
                println!("subi R{}, {}", x_reg, imm4);
            }
            // andi
            0b1_0111 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]) & imm4);
                println!("andi R{}, {}", x_reg, imm4);
            }
            // lsli
            0b1_1000 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]) << imm4);
                println!("lsli R{}, {}", x_reg, imm4);
            }
            // lsri
            0b1_1001 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]) >> imm4);
                println!("lsri R{}, {}", x_reg, imm4);
            }
            // bseti
            0b1_1010 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]) | (1 << imm4));
                println!("bseti R{}, {}", x_reg, imm4);
            }
            // bclri
            0b1_1011 => {
                self.registers[x_reg as usize] =
                    Some(unwrap_register(self.registers[x_reg as usize]) & !(1 << imm4));
                println!("bclri R{}, {}", x_reg, imm4);
            }
            _ => {
                println!("Unknown ALU opcode {:04x}", alu_opc);
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

pub struct DCoreMinimalMMU {
    pub rom_chip: [Option<u8>; 32768],
    pub ram_chip: [Option<u8>; 32768],
}

impl MMU for DCoreMinimalMMU {
    fn write_halfword(&mut self, addr: u16, value: u16) {
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

    fn read_halfword(&self, addr: u16) -> Option<u16> {
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

    fn setup_write_halfword(&mut self, _addr: u16, _value: u16) {
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

    fn setup_read_halfword(&mut self, _addr: u16) -> Option<u16> {
        self.read_halfword(_addr)
    }

    fn dump_memory(&self, start_addr: u16, end_addr: u16) -> Vec<u16> {
        let mut memory_dump = Vec::new();
        for addr in (start_addr..end_addr).step_by(2) {
            memory_dump.push(self.read_halfword(addr).unwrap_or(0));
        }
        memory_dump
    }
}
