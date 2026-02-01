use anyhow::bail;

use crate::asm_parser::{Atom, DCoreAsmAST};
use std::collections::HashMap;

pub fn setup_cpu_with_ast(cpu: &mut d_core::DCoreCPU, ast: Vec<DCoreAsmAST>) -> anyhow::Result<()> {
    let mut label_table = HashMap::<String, usize>::new();

    // Symbol Resolution
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
                    assembly_pointer += s.len() * 2;
                }
            }
            DCoreAsmAST::Push(_) | DCoreAsmAST::Pop(_) => {
                assembly_pointer += 4;
            }
            DCoreAsmAST::Stack(_) | DCoreAsmAST::Equ(_) | DCoreAsmAST::End => {}
            _ => {
                assembly_pointer += 2;
            }
        }
    }

    // Code Generation
    assembly_pointer = 0;
    let mut stack_register = 0;

    for instr in ast {
        match instr {
            DCoreAsmAST::LabelDef(_) => {}

            DCoreAsmAST::PrDez(atom) => {
                if let Atom::Register(reg) = *atom {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x0800 | (reg & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!(".prdez expects a register as argument");
                }
            }
            DCoreAsmAST::PrNewLine => {
                cpu.mmu
                    .setup_write_halfword(assembly_pointer as u16, 0x0830);
                assembly_pointer += 2;
            }
            DCoreAsmAST::PrStr(atom) => {
                if let Atom::Register(reg) = *atom {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x0820 | (reg & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!(".prstr expects a register as argument");
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
                    bail!("mov expects two registers as arguments");
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
                    bail!("addu expects two registers as arguments");
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
                    bail!("addc expects two registers as arguments");
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
                    bail!("subu expects two registers as arguments");
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
                    bail!("and expects two registers as arguments");
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
                    bail!("or expects two registers as arguments");
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
                    bail!("xor expects two registers as arguments");
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
                    bail!("not expects a register as argument");
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
                    bail!("lsl expects two registers as arguments");
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
                    bail!("lsr expects two registers as arguments");
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
                    bail!("asr expects two registers as arguments");
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
                    bail!("lslc expects a register as argument");
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
                    bail!("lsrc expects a register as argument");
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
                    bail!("asrc expects a register as argument");
                }
            }
            DCoreAsmAST::CmpE(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3000 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!("cmpe expects two registers as arguments");
                }
            }
            DCoreAsmAST::CmpNE(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3100 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!("cmpne expects two registers as arguments");
                }
            }
            DCoreAsmAST::CmpGT(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3200 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!("cmpgt expects two registers as arguments");
                }
            }
            DCoreAsmAST::CmpLT(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Register(reg_y)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3300 | ((reg_y & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!("cmplt expects two registers as arguments");
                }
            }

            DCoreAsmAST::MovI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3400 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!("movi expects a register and an immediate as arguments");
                }
            }
            DCoreAsmAST::AddI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3500 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!("addi expects a register and an immediate as arguments");
                }
            }
            DCoreAsmAST::SubI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3600 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!("subi expects a register and an immediate as arguments");
                }
            }
            DCoreAsmAST::AndI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3700 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!("andi expects a register and an immediate as arguments");
                }
            }
            DCoreAsmAST::LslI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3800 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!("lsli expects a register and an immediate as arguments");
                }
            }
            DCoreAsmAST::LsrI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3900 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!("lsri expects a register and an immediate as arguments");
                }
            }
            DCoreAsmAST::BSetI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3A00 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!("bseti expects a register and an immediate as arguments");
                }
            }
            DCoreAsmAST::BClrI(atom, atom1) => {
                if let (Atom::Register(reg_x), Atom::Number(imm4)) = (*atom, *atom1) {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3B00 | ((imm4 & 0x000F) << 4) as u16 | (reg_x & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!("bclri expects a register and an immediate as arguments");
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
                    bail!("ldw expects a register, an immediate, and a register as arguments");
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
                    bail!("stw expects a register, an immediate, and a register as arguments");
                }
            }

            DCoreAsmAST::Br(atom) => {
                if let Atom::Label(label) = *atom {
                    if let Some(&label_address) = label_table.get(&label) {
                        let pc_offset = label_address as isize - (assembly_pointer as isize + 2);
                        let imm12 = (pc_offset as i16) & 0x0FFF;
                        cpu.mmu.setup_write_halfword(
                            assembly_pointer as u16,
                            0x8000 | (imm12 as u16 & 0x0FFF),
                        );
                        assembly_pointer += 2;
                    } else {
                        bail!("Undefined label: {}", label);
                    }
                } else {
                    bail!("br expects a label as argument");
                }
            }
            DCoreAsmAST::Jsr(atom) => {
                if let Atom::Label(label) = *atom {
                    if let Some(&label_address) = label_table.get(&label) {
                        let pc_offset = label_address as isize - (assembly_pointer as isize + 2);
                        let imm12 = (pc_offset as i16) & 0x0FFF;
                        cpu.mmu.setup_write_halfword(
                            assembly_pointer as u16,
                            0x9000 | (imm12 as u16 & 0x0FFF),
                        );
                        assembly_pointer += 2;
                    } else {
                        bail!("Undefined label: {}", label);
                    }
                } else {
                    bail!("jsr expects a label as argument");
                }
            }
            DCoreAsmAST::Bt(atom) => {
                if let Atom::Label(label) = *atom {
                    if let Some(&label_address) = label_table.get(&label) {
                        let pc_offset = label_address as isize - (assembly_pointer as isize + 2);
                        let imm12 = (pc_offset as i16) & 0x0FFF;
                        cpu.mmu.setup_write_halfword(
                            assembly_pointer as u16,
                            0xA000 | (imm12 as u16 & 0x0FFF),
                        );
                        assembly_pointer += 2;
                    } else {
                        bail!("Undefined label: {}", label);
                    }
                } else {
                    bail!("bt expects a label as argument");
                }
            }
            DCoreAsmAST::Bf(atom) => {
                if let Atom::Label(label) = *atom {
                    if let Some(&label_address) = label_table.get(&label) {
                        let pc_offset = label_address as isize - (assembly_pointer as isize + 2);
                        let imm12 = (pc_offset as i16) & 0x0FFF;
                        cpu.mmu.setup_write_halfword(
                            assembly_pointer as u16,
                            0xB000 | (imm12 as u16 & 0x0FFF),
                        );
                        assembly_pointer += 2;
                    } else {
                        bail!("Undefined label: {}", label);
                    }
                } else {
                    bail!("bf expects a label as argument");
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
                    bail!("jmp expects a register as argument");
                }
            }
            DCoreAsmAST::Halt => {
                cpu.mmu
                    .setup_write_halfword(assembly_pointer as u16, 0xF000);
                assembly_pointer += 2;
            }
            DCoreAsmAST::Push(atom) => {
                // subi stack_register, 2
                // stw Ry, (stack_register)
                if let Atom::Register(reg_y) = *atom {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3600 | ((2 & 0x000F) << 4) as u16 | (stack_register & 0x000F) as u16,
                    );
                    assembly_pointer += 2;

                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x5000 | ((stack_register & 0x000F) << 4) as u16 | (reg_y & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!(".push expects a register as argument");
                }
            }
            DCoreAsmAST::Pop(atom) => {
                // ldw Ry, (stack_register)
                // addi stack_register, 2
                if let Atom::Register(reg_y) = *atom {
                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x4000 | ((stack_register & 0x000F) << 4) as u16 | (reg_y & 0x000F) as u16,
                    );
                    assembly_pointer += 2;

                    cpu.mmu.setup_write_halfword(
                        assembly_pointer as u16,
                        0x3500 | ((2 & 0x000F) << 4) as u16 | (stack_register & 0x000F) as u16,
                    );
                    assembly_pointer += 2;
                } else {
                    bail!(".pop expects a register as argument");
                }
            }
            DCoreAsmAST::Stack(atom) => {
                if let Atom::Register(reg_num) = *atom {
                    stack_register = reg_num;
                } else {
                    bail!(".stack expects a number as argument");
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
                    bail!(".defw expects a number as argument");
                }
            }
            DCoreAsmAST::DefS(atom) => {
                if let Atom::Number(size) = *atom {
                    assembly_pointer += size as usize;
                } else {
                    bail!(".defs expects a number as argument");
                }
            }
            DCoreAsmAST::Org(atom) => {
                if let Atom::Number(address) = *atom {
                    assembly_pointer = address as usize;
                } else {
                    bail!(".org expects a number as argument");
                }
            }
            DCoreAsmAST::Equ(_) => todo!(),
            DCoreAsmAST::Ascii(atom) => {
                if let Atom::String(s) = *atom {
                    cpu.mmu.write_string(assembly_pointer as u16, &s);
                    assembly_pointer += s.len() * 2;
                } else {
                    bail!(".ascii expects a string as argument");
                }
            }
            DCoreAsmAST::Assho(atom) => {
                if let Atom::String(s) = *atom {
                    cpu.mmu.write_string(assembly_pointer as u16, &s);
                    assembly_pointer += s.len() * 2;
                } else {
                    bail!(".assho expects a string as argument");
                }
            }
            DCoreAsmAST::End => break,
        }
    }
    Ok(())
}
