use chumsky::prelude::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Register(u8),
    Label(String),
    Number(i64),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum DCoreAsmAST {
    LabelDef(String),

    // Debug Operations
    /// .prdez RX
    PrDez(Box<Atom>),
    /// .prnewline
    PrNewLine,
    /// .prstr RX
    PrStr(Box<Atom>),

    // ALU Operations
    /// mov RY, RX
    Mov(Box<Atom>, Box<Atom>),
    /// addu RY, RX
    AddU(Box<Atom>, Box<Atom>),
    /// addc RY, RX
    AddC(Box<Atom>, Box<Atom>),
    /// subu RY, RX
    SubU(Box<Atom>, Box<Atom>),
    /// and RY, RX
    And(Box<Atom>, Box<Atom>),
    /// or RY, RX
    Or(Box<Atom>, Box<Atom>),
    /// xor RY, RX
    Xor(Box<Atom>, Box<Atom>),
    /// not RX
    Not(Box<Atom>),

    // Shift Operations
    /// lsl RX, RY
    Lsl(Box<Atom>, Box<Atom>),
    /// lsr RX, RY
    Lsr(Box<Atom>, Box<Atom>),
    /// asr RX, RY
    Asr(Box<Atom>, Box<Atom>),
    /// lslc RX
    LslC(Box<Atom>),
    /// lsrc RX
    LsrC(Box<Atom>),
    /// asrc RX
    AsrC(Box<Atom>),

    // Compare Operations
    /// cmpe RX, RY
    CmpE(Box<Atom>, Box<Atom>),
    /// cmpne RX, RY
    CmpNE(Box<Atom>, Box<Atom>),
    /// cmpgt RX, RY
    CmpGT(Box<Atom>, Box<Atom>),
    /// cmplt RX, RY
    CmpLT(Box<Atom>, Box<Atom>),

    // Immediate Operations
    /// movi RX, IMM4
    MovI(Box<Atom>, Box<Atom>),
    /// addi RX, IMM4
    AddI(Box<Atom>, Box<Atom>),
    /// subi RX, IMM4
    SubI(Box<Atom>, Box<Atom>),
    /// andi RX, IMM4
    AndI(Box<Atom>, Box<Atom>),
    /// lsli RX, IMM4
    LslI(Box<Atom>, Box<Atom>),
    /// lsri RX, IMM4
    LsrI(Box<Atom>, Box<Atom>),
    /// bseti RX, IMM4
    BSetI(Box<Atom>, Box<Atom>),
    /// bclri RX, IMM4
    BClrI(Box<Atom>, Box<Atom>),

    // Memory Operations
    /// ldw RX, IMM4(RY)
    LdW(Box<Atom>, Box<Atom>, Box<Atom>),
    /// stw RX, IMM4(RY)
    StW(Box<Atom>, Box<Atom>, Box<Atom>),

    // Control Flow Operations
    /// br IMM12
    Br(Box<Atom>),
    /// jsr IMM12
    Jsr(Box<Atom>),
    /// bt IMM12
    Bt(Box<Atom>),
    /// bf IMM12
    Bf(Box<Atom>),
    /// jmp RX
    Jmp(Box<Atom>),
    /// halt
    Halt,

    // Stack Operations
    Push(Box<Atom>),
    Pop(Box<Atom>),
    Stack(Box<Atom>),

    // Interupt Operations
    Trap(Box<Atom>),
    Eepc,
    Rfi,

    DefW(Box<Atom>),  // Define Word
    DefS(Box<Atom>),  // Reserver for arrays etc.
    Org(Box<Atom>),   // Move assembly pointer
    Equ(Box<Atom>),   // Define compilation constant
    Ascii(Box<Atom>), // Define ASCII string
    Assho(Box<Atom>), // Define ASCII string
    End,
}

pub fn parser<'src>() -> impl Parser<'src, &'src str, Vec<DCoreAsmAST>> {
    let comment = just(";")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    let hex_prefix = just("0x").or(just("$"));

    // Parse Registers: R0 to R15
    let register = just('r')
        .ignore_then(text::int(10))
        .padded()
        .map(|s: &str| s.parse::<u8>().unwrap())
        .filter(|&num| num <= 15)
        .map(Atom::Register);

    let no_param_instr = choice((
        just(".prnewline"),
        just("halt"),
        // just("eepc"),
        just("rfi"),
        just(".end"),
    ))
    .map(|s| match s {
        ".prnewline" => DCoreAsmAST::PrNewLine,
        "halt" => DCoreAsmAST::Halt,
        // "eepc" => DCoreAsmAST::Eepc,
        "rfi" => DCoreAsmAST::Rfi,
        ".end" => DCoreAsmAST::End,
        _ => unreachable!(),
    });

    let one_register_instr = choice((
        just(".prdez"),
        just(".prstr"),
        just("not"),
        just("lslc"),
        just("lsrc"),
        just("asrc"),
        just("jmp"),
        just(".push"),
        just(".pop"),
        just(".stack"),
    ))
    .padded()
    .then(register)
    .map(|(identifier, reg)| match identifier {
        ".prdez" => DCoreAsmAST::PrDez(Box::new(reg)),
        ".prstr" => DCoreAsmAST::PrStr(Box::new(reg)),
        "not" => DCoreAsmAST::Not(Box::new(reg)),
        "lslc" => DCoreAsmAST::LslC(Box::new(reg)),
        "lsrc" => DCoreAsmAST::LsrC(Box::new(reg)),
        "asrc" => DCoreAsmAST::AsrC(Box::new(reg)),
        "jmp" => DCoreAsmAST::Jmp(Box::new(reg)),
        ".push" => DCoreAsmAST::Push(Box::new(reg)),
        ".pop" => DCoreAsmAST::Pop(Box::new(reg)),
        ".stack" => DCoreAsmAST::Stack(Box::new(reg)),
        _ => unreachable!(),
    });

    let two_register_instr = choice((
        just("mov"),
        just("addu"),
        just("addc"),
        just("subu"),
        just("and"),
        just("or"),
        just("xor"),
        just("lsl"),
        just("lsr"),
        just("asr"),
        just("cmpe"),
        just("cmpne"),
        just("cmpgt"),
        just("cmplt"),
    ))
    .padded()
    .then(register)
    .then(just(',').padded().ignore_then(register))
    .map(|((identifier, reg1), reg2)| match identifier {
        "mov" => DCoreAsmAST::Mov(Box::new(reg1), Box::new(reg2)),
        "addu" => DCoreAsmAST::AddU(Box::new(reg1), Box::new(reg2)),
        "addc" => DCoreAsmAST::AddC(Box::new(reg1), Box::new(reg2)),
        "subu" => DCoreAsmAST::SubU(Box::new(reg1), Box::new(reg2)),
        "and" => DCoreAsmAST::And(Box::new(reg1), Box::new(reg2)),
        "or" => DCoreAsmAST::Or(Box::new(reg1), Box::new(reg2)),
        "xor" => DCoreAsmAST::Xor(Box::new(reg1), Box::new(reg2)),
        "lsl" => DCoreAsmAST::Lsl(Box::new(reg1), Box::new(reg2)),
        "lsr" => DCoreAsmAST::Lsr(Box::new(reg1), Box::new(reg2)),
        "asr" => DCoreAsmAST::Asr(Box::new(reg1), Box::new(reg2)),
        "cmpe" => DCoreAsmAST::CmpE(Box::new(reg1), Box::new(reg2)),
        "cmpne" => DCoreAsmAST::CmpNE(Box::new(reg1), Box::new(reg2)),
        "cmpgt" => DCoreAsmAST::CmpGT(Box::new(reg1), Box::new(reg2)),
        "cmplt" => DCoreAsmAST::CmpLT(Box::new(reg1), Box::new(reg2)),
        _ => unreachable!(),
    });

    // Immediate value may be decimal or hexadecimal
    let immediate = hex_prefix
        .ignore_then(text::int(16))
        .map(|s: &str| Atom::Number(i64::from_str_radix(s, 16).unwrap()))
        .or(text::int(10).map(|s: &str| Atom::Number(s.parse::<i64>().unwrap())))
        .padded();

    let one_immediate_instr = choice((just(".defw"), just(".org"), just(".equ")))
        .padded()
        .then(immediate)
        .map(|(identifier, imm)| match identifier {
            ".defw" => DCoreAsmAST::DefW(Box::new(imm)),
            ".defs" => DCoreAsmAST::DefS(Box::new(imm)),
            ".org" => DCoreAsmAST::Org(Box::new(imm)),
            ".equ" => DCoreAsmAST::Equ(Box::new(imm)),
            _ => unreachable!(),
        });

    let one_register_one_immediate_instr = choice((
        just("movi"),
        just("addi"),
        just("subi"),
        just("andi"),
        just("lsli"),
        just("lsri"),
        just("bseti"),
        just("bclri"),
    ))
    .padded()
    .then(register)
    .then_ignore(just(',').padded())
    .then(immediate)
    .map(|((identifier, reg), imm)| match identifier {
        "movi" => DCoreAsmAST::MovI(Box::new(reg), Box::new(imm)),
        "addi" => DCoreAsmAST::AddI(Box::new(reg), Box::new(imm)),
        "subi" => DCoreAsmAST::SubI(Box::new(reg), Box::new(imm)),
        "andi" => DCoreAsmAST::AndI(Box::new(reg), Box::new(imm)),
        "lsli" => DCoreAsmAST::LslI(Box::new(reg), Box::new(imm)),
        "lsri" => DCoreAsmAST::LsrI(Box::new(reg), Box::new(imm)),
        "bseti" => DCoreAsmAST::BSetI(Box::new(reg), Box::new(imm)),
        "bclri" => DCoreAsmAST::BClrI(Box::new(reg), Box::new(imm)),
        _ => unreachable!(),
    });

    let branch_instr = choice((
        just("br"),
        just("jsr"),
        just("bt"),
        just("bf"),
        just("trap"),
    ))
    .padded()
    .then(choice((
        text::ident()
            .padded()
            .map(|s: &str| Atom::Label(s.to_string())),
        immediate,
    )))
    .map(|(identifier, imm)| match identifier {
        "br" => DCoreAsmAST::Br(Box::new(imm)),
        "jsr" => DCoreAsmAST::Jsr(Box::new(imm)),
        "bt" => DCoreAsmAST::Bt(Box::new(imm)),
        "bf" => DCoreAsmAST::Bf(Box::new(imm)),
        "trap" => DCoreAsmAST::Trap(Box::new(imm)),
        _ => unreachable!(),
    });

    let label_definition = text::ident()
        .then_ignore(just(':'))
        .padded()
        .map(|s: &str| s.to_string())
        .map(DCoreAsmAST::LabelDef);

    // Ascii string defintion
    let ascii_string = choice((just(".ascii"), just(".assho")))
        .padded()
        .then(
            just('"')
                .ignore_then(any().filter(|c| *c != '"').repeated().collect::<String>())
                .then_ignore(just('"'))
                .padded(),
        )
        .map(|(identifier, string)| match identifier {
            ".ascii" => DCoreAsmAST::Ascii(Box::new(Atom::String(string))),
            ".assho" => DCoreAsmAST::Assho(Box::new(Atom::String(string))),
            _ => unreachable!(),
        });

    // ldw R0, 0(R1)
    // stw R5, 4(R8)
    // ldw R2, -21(R2)
    // ldw R3, 0x10(R4)
    // stw R7, $FF(R0)
    let memory_instr = choice((just("ldw"), just("stw")))
        .padded()
        .then(register)
        .then_ignore(just(',').padded())
        .then(
            immediate
                .then_ignore(just('(').padded())
                .then(register)
                .then_ignore(just(')').padded()),
        )
        .map(|((identifier, reg1), (imm, reg2))| match identifier {
            "ldw" => DCoreAsmAST::LdW(Box::new(reg1), Box::new(imm), Box::new(reg2)),
            "stw" => DCoreAsmAST::StW(Box::new(reg1), Box::new(imm), Box::new(reg2)),
            _ => unreachable!(),
        });

    let instruction = choice((
        no_param_instr,
        one_register_instr,
        two_register_instr,
        one_immediate_instr,
        one_register_one_immediate_instr,
        label_definition,
        memory_instr,
        ascii_string,
        branch_instr,
    ));

    instruction
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser() {
        assert!(parser().parse("").into_result().is_ok());
    }

    #[test]
    fn test_towers() {
        let code = "
_start:
       .stack r0
       movi R1, 0   ; Global C = R1
       movi R5, 0
       movi R6, 0


       movi R2, 8   ; ABI n, i, j
       movi R3, 1
       movi R4, 3
       jsr towers
       halt


; N = R2
; I = R3
; J = R4
; R5 = 1
; K = R6
towers:
       .push R2
       .push R3
       .push R4
       .push R5
       .push R6
       .push R15

       movi R5, 1
       cmpe R5, R2
       bf iffalse
       addi R1, 1

       jsr ldr7     ; Schritt
       .defw 0x9000

       .prstr R7
       .prdez R1

       jsr ldr7     ; :
       .defw 0x9020

       .prstr R7
       .prdez R3

       jsr ldr7     ;  ->
       .defw 0x9040

       .prstr R7
       .prdez R4

       .prnewline
       br ifend
iffalse:
       movi R6, 6
       subu R6, R3
       subu R6, R4


       .push R2
       .push R4
       subi R2, 1
       mov R4, R6
       jsr towers
       .pop R4
       .pop R2

       .push R2
       movi R2, 1
       jsr towers
       .pop R2

       .push R2
       .push R3
       subi R2, 1
       mov R3, R6
       jsr towers
       .pop R3
       .pop R2
ifend:
      .pop R15
      .pop R6
      .pop R5
      .pop R4
      .pop R3
      .pop R2

      jmp R15

; R7 = output
ldr7:
     ldw R7, 0(R15)
     addi R15, 2

     jmp R15

.org 0x9000
.ascii \"Schritt \"
.defw 0x0

.org 0x9020
.ascii \": \"
.defw 0x0

.org 0x9040
.ascii \" -> \"
.defw 0x0
            "
        .to_lowercase();
        assert!(parser().parse(&code).into_result().is_ok());
    }
}
