use d_core::DCoreCPU;
use std::thread;

fn main() {
    let mut core = DCoreCPU::new();
    // Row 1 (0x0000)
    core.mmu.setup_write_halfword(0x0000, 0x3400);
    core.mmu.setup_write_halfword(0x0002, 0x3510);
    core.mmu.setup_write_halfword(0x0004, 0x8ffc);

    while !core.halted {
        println!();
        println!("PC: {:04x}", core.pc);
        for reg in core.registers {
            print!("{:04x} ", reg.unwrap_or(0));
        }
        println!("");
        core.step();
    }
}
