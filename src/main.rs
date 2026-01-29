use std::io::Write;

use d_core::DCoreCPU;

fn main() {
    let mut core = DCoreCPU::new();

    // Row 1 (0x0000)
    core.mmu.setup_write_halfword(0x0000, 0x3401);
    core.mmu.setup_write_halfword(0x0002, 0x3405);
    core.mmu.setup_write_halfword(0x0004, 0x3406);
    core.mmu.setup_write_halfword(0x0006, 0x3482);
    core.mmu.setup_write_halfword(0x0008, 0x3413);

    // Row 2 (0x000A)
    core.mmu.setup_write_halfword(0x000A, 0x3434);
    core.mmu.setup_write_halfword(0x000C, 0x9002);
    core.mmu.setup_write_halfword(0x000E, 0xF000);
    core.mmu.setup_write_halfword(0x0010, 0x3620);
    core.mmu.setup_write_halfword(0x0012, 0x5002);

    // Row 3 (0x0014)
    core.mmu.setup_write_halfword(0x0014, 0x3620);
    core.mmu.setup_write_halfword(0x0016, 0x5003);
    core.mmu.setup_write_halfword(0x0018, 0x3620);
    core.mmu.setup_write_halfword(0x001A, 0x5004);
    core.mmu.setup_write_halfword(0x001C, 0x3620);

    // Row 4 (0x001E)
    core.mmu.setup_write_halfword(0x001E, 0x5005);
    core.mmu.setup_write_halfword(0x0020, 0x3620);
    core.mmu.setup_write_halfword(0x0022, 0x5006);
    core.mmu.setup_write_halfword(0x0024, 0x3620);
    core.mmu.setup_write_halfword(0x0026, 0x500F);

    // Row 5 (0x0028)
    core.mmu.setup_write_halfword(0x0028, 0x3415);
    core.mmu.setup_write_halfword(0x002A, 0x3025);
    core.mmu.setup_write_halfword(0x002C, 0xB01E);
    core.mmu.setup_write_halfword(0x002E, 0x3511);
    core.mmu.setup_write_halfword(0x0030, 0x9072);

    // Row 6 (0x0032)
    core.mmu.setup_write_halfword(0x0032, 0x9000);
    core.mmu.setup_write_halfword(0x0034, 0x0827);
    core.mmu.setup_write_halfword(0x0036, 0x0801);
    core.mmu.setup_write_halfword(0x0038, 0x906A);
    core.mmu.setup_write_halfword(0x003A, 0x9020);

    // Row 7 (0x003C)
    core.mmu.setup_write_halfword(0x003C, 0x0827);
    core.mmu.setup_write_halfword(0x003E, 0x0803);
    core.mmu.setup_write_halfword(0x0040, 0x9062);
    core.mmu.setup_write_halfword(0x0042, 0x9040);
    core.mmu.setup_write_halfword(0x0044, 0x0827);

    // Row 8 (0x0046)
    core.mmu.setup_write_halfword(0x0046, 0x0804);
    core.mmu.setup_write_halfword(0x0048, 0x0830);
    core.mmu.setup_write_halfword(0x004A, 0x803E);
    core.mmu.setup_write_halfword(0x004C, 0x3466);
    core.mmu.setup_write_halfword(0x004E, 0x2336);

    // Row 9 (0x0050)
    core.mmu.setup_write_halfword(0x0050, 0x2346);
    core.mmu.setup_write_halfword(0x0052, 0x3620);
    core.mmu.setup_write_halfword(0x0054, 0x5002);
    core.mmu.setup_write_halfword(0x0056, 0x3620);
    core.mmu.setup_write_halfword(0x0058, 0x5004);

    // Row 10 (0x005A)
    core.mmu.setup_write_halfword(0x005A, 0x3612);
    core.mmu.setup_write_halfword(0x005C, 0x2064);
    core.mmu.setup_write_halfword(0x005E, 0x9FB0);
    core.mmu.setup_write_halfword(0x0060, 0x4004);
    core.mmu.setup_write_halfword(0x0062, 0x3520);

    // Row 11 (0x0064)
    core.mmu.setup_write_halfword(0x0064, 0x4002);
    core.mmu.setup_write_halfword(0x0066, 0x3520);
    core.mmu.setup_write_halfword(0x0068, 0x3620);
    core.mmu.setup_write_halfword(0x006A, 0x5002);
    core.mmu.setup_write_halfword(0x006C, 0x3412);

    // Row 12 (0x006E)
    core.mmu.setup_write_halfword(0x006E, 0x9FA0);
    core.mmu.setup_write_halfword(0x0070, 0x4002);
    core.mmu.setup_write_halfword(0x0072, 0x3520);
    core.mmu.setup_write_halfword(0x0074, 0x3620);
    core.mmu.setup_write_halfword(0x0076, 0x5002);

    // Row 13 (0x0078)
    core.mmu.setup_write_halfword(0x0078, 0x3620);
    core.mmu.setup_write_halfword(0x007A, 0x5003);
    core.mmu.setup_write_halfword(0x007C, 0x3612);
    core.mmu.setup_write_halfword(0x007E, 0x2063);
    core.mmu.setup_write_halfword(0x0080, 0x9F8E);

    // Row 14 (0x0082)
    core.mmu.setup_write_halfword(0x0082, 0x4003);
    core.mmu.setup_write_halfword(0x0084, 0x3520);
    core.mmu.setup_write_halfword(0x0086, 0x4002);
    core.mmu.setup_write_halfword(0x0088, 0x3520);
    core.mmu.setup_write_halfword(0x008A, 0x400F);

    // Row 15 (0x008C)
    core.mmu.setup_write_halfword(0x008C, 0x3520);
    core.mmu.setup_write_halfword(0x008E, 0x4006);
    core.mmu.setup_write_halfword(0x0090, 0x3520);
    core.mmu.setup_write_halfword(0x0092, 0x4005);
    core.mmu.setup_write_halfword(0x0094, 0x3520);

    // Row 16 (0x0096)
    core.mmu.setup_write_halfword(0x0096, 0x4004);
    core.mmu.setup_write_halfword(0x0098, 0x3520);
    core.mmu.setup_write_halfword(0x009A, 0x4003);
    core.mmu.setup_write_halfword(0x009C, 0x3520);
    core.mmu.setup_write_halfword(0x009E, 0x4002);

    // Row 17 (0x00A0)
    core.mmu.setup_write_halfword(0x00A0, 0x3520);
    core.mmu.setup_write_halfword(0x00A2, 0xC00F);
    core.mmu.setup_write_halfword(0x00A4, 0x40F7);
    core.mmu.setup_write_halfword(0x00A6, 0x352F);
    core.mmu.setup_write_halfword(0x00A8, 0xC00F);

    while !core.halted {
        let mut outstring = String::from("\n");

        outstring.push_str(&format!("PC: {:04x}\n", core.pc));
        for reg in core.registers {
            outstring.push_str(&format!("{:04x} ", reg.unwrap_or(0)));
        }
        outstring.push('\n');
        print!("{}", outstring);
        std::io::stdout().flush().unwrap();
        core.step();
        // let _ = std::io::stdin().read_line(&mut String::new());
    }
}
