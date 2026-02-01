mod app;
mod asm_parser;
mod assembler;

use crate::app::App;
use chumsky::prelude::*;
use d_core::DCoreCPU;

#[cfg(not(target_arch = "wasm32"))]
fn main() {
    use crate::assembler::setup_cpu_with_ast;

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

    let mut cpu = DCoreCPU::new();

    let ast = crate::asm_parser::parser().parse(&code);
    println!("AST: {:#?}", ast);
    match ast.into_result() {
        Ok(ast) => setup_cpu_with_ast(&mut cpu, ast),
        Err(e) => println!("Parse error: {:?}", e),
    }
    for v in cpu.mmu.dump_memory(0x0000, 0x0060) {
        print!("{:02X} ", v);
    }
    for v in cpu.mmu.dump_memory(0x9000, 0x9060) {
        print!("{:02X} ", v);
    }

    // println!("Starting T3 Assembler...");
    //
    // let native_options = eframe::NativeOptions {
    //     viewport: egui::ViewportBuilder::default()
    //         .with_title("T3 Assembler")
    //         .with_decorations(true),
    //     ..Default::default()
    // };
    // eframe::run_native(
    //     "T3 Assembler",
    //     native_options,
    //     Box::new(|_cc| Ok(Box::new(App::new(_cc)))),
    // )
    // .expect("Failed to run native application");
}

#[cfg(target_arch = "wasm32")]
fn main() {
    use eframe::wasm_bindgen::JsCast as _;

    // Redirect `log` message to `console.log` and friends:
    eframe::WebLogger::init(log::LevelFilter::Debug).ok();

    let web_options = eframe::WebOptions::default();

    wasm_bindgen_futures::spawn_local(async {
        let document = web_sys::window()
            .expect("No window")
            .document()
            .expect("No document");

        let canvas = document
            .get_element_by_id("the_canvas_id")
            .expect("Failed to find the_canvas_id")
            .dyn_into::<web_sys::HtmlCanvasElement>()
            .expect("the_canvas_id was not a HtmlCanvasElement");

        let start_result = eframe::WebRunner::new()
            .start(
                canvas,
                web_options,
                Box::new(|_cc| Ok(Box::new(App::new(_cc)))),
            )
            .await;

        // Remove the loading text and spinner:
        if let Some(loading_text) = document.get_element_by_id("loading_text") {
            match start_result {
                Ok(_) => {
                    loading_text.remove();
                }
                Err(e) => {
                    loading_text.set_inner_html(
                        "<p> The app has crashed. See the developer console for details. </p>",
                    );
                    panic!("Failed to start eframe: {e:?}");
                }
            }
        }
    });
}
