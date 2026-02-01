use crate::asm_parser::{Atom, DCoreAsmAST};
use std::collections::HashMap;

pub fn setup_cpu_with_ast(cpu: &mut d_core::DCoreCPU, ast: Vec<DCoreAsmAST>) {
    let mut label_table = HashMap::<String, usize>::new();

    // =========================================================================
    // PASS 1: Symbol Resolution & Address Calculation
    // =========================================================================
    // We iterate over references so we don't consume the AST.
    // We calculate where labels land based on instruction sizes.
    let mut assembly_pointer = 0usize;

    for instr in &ast {
        match instr {
            DCoreAsmAST::LabelDef(label) => {
                label_table.insert(label.clone(), assembly_pointer);
            }
            DCoreAsmAST::Org(atom) => {
                if let Atom::Number(address) = **atom {
                    assembly_pointer = address as usize;
                }
            }
            DCoreAsmAST::Ascii(atom) => {
                if let Atom::String(s) = *atom.clone() {
                    // Original logic: 1 char = 1 halfword (2 bytes)
                    assembly_pointer += s.len() * 2;
                }
            }
            DCoreAsmAST::Push(_) | DCoreAsmAST::Pop(_) => {
                // These macro-instructions emit 2 CPU instructions (4 bytes)
                assembly_pointer += 4;
            }
            // Zero-width directives
            DCoreAsmAST::Stack(_) | DCoreAsmAST::Equ(_) | DCoreAsmAST::End => {}
            // All other standard instructions are 1 halfword (2 bytes)
            _ => {
                assembly_pointer += 2;
            }
        }
    }

    // =========================================================================
    // PASS 2: Code Generation
    // =========================================================================
    assembly_pointer = 0; // Reset pointer for generation
    let mut stack_register = 0;

    for instr in ast {
        match instr {
            // Already handled in Pass 1, do nothing here
            DCoreAsmAST::LabelDef(_) => {}

            DCoreAsmAST::PrDez(atom) => {
                if let Atom::Register(reg) = *atom {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x8000 | (reg & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!(".prdez expects a register as argument");
                }
            }
            DCoreAsmAST::PrNewLine => {
                cpu.mmu
                    .setup_write_halfword(assembly_pointer as u16, 0x8300);
                assembly_pointer += 2;
            }
            DCoreAsmAST::PrStr(atom) => {
                if let Atom::Register(reg) = *atom {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x8200 | (reg & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!(".prstr expects a register as argument");
                }
            }
            DCoreAsmAST::Mov(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x2000 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("mov expects two registers as arguments");
                }
            }
            DCoreAsmAST::AddU(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x2100 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("addu expects two registers as arguments");
                }
            }
            DCoreAsmAST::AddC(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x2200 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("addc expects two registers as arguments");
                }
            }
            DCoreAsmAST::SubU(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x2300 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("subu expects two registers as arguments");
                }
            }
            DCoreAsmAST::And(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x2400 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("and expects two registers as arguments");
                }
            }
            DCoreAsmAST::Or(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x2500 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("or expects two registers as arguments");
                }
            }
            DCoreAsmAST::Xor(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x2600 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("xor expects two registers as arguments");
                }
            }
            DCoreAsmAST::Not(atom) => {
                if let Atom::Register(reg_x) = *atom {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x2700 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("not expects a register as argument");
                }
            }
            DCoreAsmAST::Lsl(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x2800 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("lsl expects two registers as arguments");
                }
            }
            DCoreAsmAST::Lsr(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x2900 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("lsr expects two registers as arguments");
                }
            }
            DCoreAsmAST::Asr(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x2A00 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("asr expects two registers as arguments");
                }
            }
            DCoreAsmAST::LslC(atom) => {
                if let Atom::Register(reg_x) = *atom {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x2C00 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("lslc expects a register as argument");
                }
            }
            DCoreAsmAST::LsrC(atom) => {
                if let Atom::Register(reg_x) = *atom {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x2D00 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("lsrc expects a register as argument");
                }
            }
            DCoreAsmAST::AsrC(atom) => {
                if let Atom::Register(reg_x) = *atom {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x2E00 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("asrc expects a register as argument");
                }
            }
            DCoreAsmAST::CmpE(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x9000 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("cmpe expects two registers as arguments");
                }
            }
            DCoreAsmAST::CmpNE(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x9100 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("cmpne expects two registers as arguments");
                }
            }
            DCoreAsmAST::CmpGT(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x9200 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("cmpgt expects two registers as arguments");
                }
            }
            DCoreAsmAST::CmpLT(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x9300 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("cmplt expects two registers as arguments");
                }
            }
            DCoreAsmAST::MovI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3000 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("movi expects a register and an immediate as arguments");
                }
            }
            DCoreAsmAST::AddI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3100 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("addi expects a register and an immediate as arguments");
                }
            }
            DCoreAsmAST::SubI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3200 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("subi expects a register and an immediate as arguments");
                }
            }
            DCoreAsmAST::AndI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3300 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("andi expects a register and an immediate as arguments");
                }
            }
            DCoreAsmAST::LslI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3400 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("lsli expects a register and an immediate as arguments");
                }
            }
            DCoreAsmAST::LsrI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3500 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("lsri expects a register and an immediate as arguments");
                }
            }
            DCoreAsmAST::BSetI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3600 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("bseti expects a register and an immediate as arguments");
                }
            }
            DCoreAsmAST::BClrI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3700 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("bclri expects a register and an immediate as arguments");
                }
            }
            DCoreAsmAST::LdW(atom, atom1, atom2) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4), Atom::Register(reg_y)) =
                    (*atom, *atom1, *atom2)
                {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x4000
                            | ((imm4 & 0x000F) << 8) as u16
                            | ((reg_y & 0x000F) << 4) as u16
                            | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("ldw expects a register, an immediate, and a register as arguments");
                }
            }
            DCoreAsmAST::StW(atom, atom1, atom2) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4), Atom::Register(reg_y)) =
                    (*atom, *atom1, *atom2)
                {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x5000
                            | ((imm4 & 0x000F) << 8) as u16
                            | ((reg_y & 0x000F) << 4) as u16
                            | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("stw expects a register, an immediate, and a register as arguments");
                }
            }
            DCoreAsmAST::Br(atom) => {
                if let Atom::Label(label) = *atom {
                    if let Some(&label_address) = label_table.get(&label) {
                        let pc_offset =
                            (label_address as isize - (assembly_pointer as isize + 2)) / 2;
                        let imm12 = (pc_offset as i16) & 0x0FFF;
                        cpu.mmu.setup_write_halfword(
                            assembly_pointer as u16,
                            0x8000 | (imm12 as u16 & 0x0FFF),
                        );
                        assembly_pointer += 2;
                    } else {
                        panic!("Undefined label: {}", label);
                    }
                } else {
                    panic!("br expects a label as argument");
                }
            }
            DCoreAsmAST::Jsr(atom) => {
                if let Atom::Label(label) = *atom {
                    if let Some(&label_address) = label_table.get(&label) {
                        let pc_offset =
                            (label_address as isize - (assembly_pointer as isize + 2)) / 2;
                        let imm12 = (pc_offset as i16) & 0x0FFF;
                        cpu.mmu.setup_write_halfword(
                            assembly_pointer as u16,
                            0x9000 | (imm12 as u16 & 0x0FFF),
                        );
                        assembly_pointer += 2;
                    } else {
                        panic!("Undefined label: {}", label);
                    }
                } else {
                    panic!("jsr expects a label as argument");
                }
            }
            DCoreAsmAST::Bt(atom) => {
                if let Atom::Label(label) = *atom {
                    if let Some(&label_address) = label_table.get(&label) {
                        let pc_offset =
                            (label_address as isize - (assembly_pointer as isize + 2)) / 2;
                        let imm12 = (pc_offset as i16) & 0x0FFF;
                        cpu.mmu.setup_write_halfword(
                            assembly_pointer as u16,
                            0xA000 | (imm12 as u16 & 0x0FFF),
                        );
                        assembly_pointer += 2;
                    } else {
                        panic!("Undefined label: {}", label);
                    }
                } else {
                    panic!("bt expects a label as argument");
                }
            }
            DCoreAsmAST::Bf(atom) => {
                if let Atom::Label(label) = *atom {
                    if let Some(&label_address) = label_table.get(&label) {
                        let pc_offset =
                            (label_address as isize - (assembly_pointer as isize + 2)) / 2;
                        let imm12 = (pc_offset as i16) & 0x0FFF;
                        cpu.mmu.setup_write_halfword(
                            assembly_pointer as u16,
                            0xB000 | (imm12 as u16 & 0x0FFF),
                        );
                        assembly_pointer += 2;
                    } else {
                        panic!("Undefined label: {}", label);
                    }
                } else {
                    panic!("bf expects a label as argument");
                }
            }
            DCoreAsmAST::Jmp(atom) => {
                if let Atom::Register(reg) = *atom {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0xC000 | (reg & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!("jmp expects a register as argument");
                }
            }
            DCoreAsmAST::Halt => {
                cpu.mmu
                    .setup_write_halfword(assembly_pointer as u16, 0xFFFF);
                assembly_pointer += 2;
            }
            DCoreAsmAST::Push(atom) => {
                // Assembles to
                // subi stack_register, 2
                // stw Ry, (stack_register)
                if let Atom::Register(reg_y) = *atom {
                    // subi stack_register, 2
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3200 | ((2 & 0x000F) << 4) as u16 | (stack_register & 0x000F) as u16,
                    );
                    assembly_pointer += 2;

                    // stw Ry, 0(stack_register)
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x5000 | ((stack_register & 0x000F) << 4) as u16 | (reg_y & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!(".push expects a register as argument");
                }
            }
            DCoreAsmAST::Pop(atom) => {
                // ldw Ry, (stack_register)
                // addi stack_register, 2
                if let Atom::Register(reg_y) = *atom {
                    // ldw Ry, 0(stack_register)
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x4000 | ((stack_register & 0x000F) << 4) as u16 | (reg_y & 0x000F) as u16,
                    );
                    assembly_pointer += 2;

                    // addi stack_register, 2
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3100 | ((2 & 0x000F) << 4) as u16 | (stack_register & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    panic!(".pop expects a register as argument");
                }
            }
            DCoreAsmAST::Stack(atom) => {
                if let Atom::Register(reg_num) = *atom {
                    stack_register = reg_num;
                } else {
                    panic!(".stack expects a number as argument");
                }
            }
            DCoreAsmAST::Trap(_) => todo!(),
            DCoreAsmAST::Eepc => todo!(),
            DCoreAsmAST::Rfi => todo!(),
            DCoreAsmAST::DefW(atom) => {
                if let Atom::Number(value) = *atom {
                    cpu.mmu
                        .setup_write_halfword(assembly_pointer as u16, value as u16);
                    assembly_pointer += 2;
                } else {
                    panic!(".defw expects a number as argument");
                }
            }
            DCoreAsmAST::DefS(atom) => {
                // Reserve space for s bytes
                if let Atom::Number(size) = *atom {
                    assembly_pointer += size as usize;
                } else {
                    panic!(".defs expects a number as argument");
                }
            }
            DCoreAsmAST::Org(atom) => {
                if let Atom::Number(address) = *atom {
                    assembly_pointer = address as usize;
                } else {
                    panic!(".org expects a number as argument");
                }
            }
            DCoreAsmAST::Equ(_) => todo!(),
            DCoreAsmAST::Ascii(atom) => {
                if let Atom::String(s) = *atom {
                    for byte in s.bytes() {
                        cpu.mmu
                            .setup_write_halfword(assembly_pointer as u16, byte as u16);
                        assembly_pointer += 2;
                    }
                } else {
                    panic!(".ascii expects a string as argument");
                }
            }
            DCoreAsmAST::Assho(atom) => {
                if let Atom::Number(value) = *atom {
                    cpu.mmu
                        .setup_write_halfword(assembly_pointer as u16, value as u16);
                    assembly_pointer += 2;
                } else {
                    panic!(".assho expects a number as argument");
                }
            }
            DCoreAsmAST::End => break,
        }
    }
}
