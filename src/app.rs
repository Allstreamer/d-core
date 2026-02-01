use chumsky::prelude::*;
use d_core::DCoreCPU;
use eframe::get_value;
use egui::{Color32, RichText, TextStyle};
use egui_extras::{Column, TableBuilder};

use crate::{asm_parser::parser, assembler::setup_cpu_with_ast};

#[derive(serde::Deserialize, serde::Serialize)]
#[serde(default)]
pub struct App {
    program: String,
    cpu: DCoreCPU,
}

impl App {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        if let Some(storage) = cc.storage {
            get_value::<Self>(storage, eframe::APP_KEY).unwrap_or_default()
        } else {
            Default::default()
        }
    }

    fn code_view(&mut self, ui: &mut egui::Ui) {
        let theme = egui_extras::syntax_highlighting::CodeTheme::from_memory(ui.ctx(), ui.style());
        let mut layouter = |ui: &egui::Ui, buf: &dyn egui::TextBuffer, wrap_width: f32| {
            let mut layout_job = egui_extras::syntax_highlighting::highlight(
                ui.ctx(),
                ui.style(),
                &theme,
                buf.as_str(),
                "d-core-asm",
            );
            layout_job.wrap.max_width = wrap_width;
            ui.fonts_mut(|f| f.layout_job(layout_job))
        };
        let background_color = if theme.is_dark() {
            Color32::BLACK
        } else {
            Color32::WHITE
        };

        let editor = egui::TextEdit::multiline(&mut self.program)
            .desired_rows(32)
            .lock_focus(true)
            .desired_width(f32::INFINITY)
            .layouter(&mut layouter)
            .background_color(background_color);

        ui.add(editor);
    }

    fn memory_view(&mut self, ui: &mut egui::Ui) {
        let ast = parser().parse(self.program.as_str());
        if let Ok(ast) = ast.into_result() {
            self.cpu.mmu.wipe_all();
            let _ = setup_cpu_with_ast(&mut self.cpu, ast);
        }

        let text_style = TextStyle::Monospace;

        let row_height = ui.text_style_height(&text_style);

        // Switch to 8 columns (16 bytes) per row. It aligns better with Hex math.

        let items_per_row = 8;

        // Total number of u16 items in 0xFFFF space

        let total_items = 0x10000 / 2;

        let total_rows = total_items / items_per_row;

        // Use a specific height or fill the available space

        ui.push_id("memory_table", |ui| {
            TableBuilder::new(ui)
                .striped(true) // easier to follow lines
                .cell_layout(egui::Layout::left_to_right(egui::Align::Center))
                .column(Column::auto().at_least(60.0)) // Address column
                .columns(Column::remainder().at_least(40.0), items_per_row) // Data columns
                .header(20.0, |mut header| {
                    header.col(|ui| {
                        ui.strong("Addr");
                    });

                    for i in 0..items_per_row {
                        header.col(|ui| {
                            ui.label(RichText::new(format!("+{:X}", i * 2)).monospace());
                        });
                    }
                })
                .body(|body| {
                    body.rows(row_height, total_rows, |mut row| {
                        let row_index = row.index();

                        let base_addr = row_index * items_per_row;

                        row.col(|ui| {
                            ui.label(
                                RichText::new(format!("0x{:04X}", base_addr * 2))
                                    .color(Color32::from_gray(140)) // Dim the address slightly
                                    .monospace(),
                            );
                        });

                        for i in 0..items_per_row {
                            let offset = base_addr + i;

                            let val = self.cpu.mmu.setup_read_halfword((offset * 2) as u16);

                            row.col(|ui| {
                                let text = if let Some(val) = val {
                                    format!("0x{:04X}", val)
                                } else {
                                    String::from("0x----")
                                };

                                let color = if val.is_none() {
                                    Color32::from_gray(70)
                                } else if let Some(0) = val {
                                    Color32::from_gray(120)
                                } else {
                                    Color32::LIGHT_GRAY
                                };

                                ui.label(RichText::new(text).color(color).monospace());
                            });
                        }
                    });
                });
        });
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.menu_button("Datei", |ui| {
                    ui.button("Neu").clicked();
                    ui.button("Öffnen").clicked();
                    ui.button("Speichern Unter").clicked();

                    let is_web = cfg!(target_arch = "wasm32");
                    if !is_web && ui.button("Quit").clicked() {
                        ctx.send_viewport_cmd(egui::ViewportCommand::Close);
                    }
                });

                ui.menu_button("Werkzeuge", |ui| {
                    ui.button("Assemblieren").clicked();
                    ui.button("Emulator").clicked();
                });

                ui.menu_button("Hilfe", |ui| {
                    ui.button("Anleitung").clicked();
                    ui.button("Info").clicked();
                });

                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                    egui::warn_if_debug_build(ui);
                    egui::widgets::global_theme_preference_buttons(ui);
                });
            });
        });

        egui::CentralPanel::default().show(ctx, |ui| {
            ui.columns(2, |ui| {
                egui::ScrollArea::vertical()
                    .id_salt("code_view")
                    .show(&mut ui[0], |ui| {
                        self.code_view(ui);
                    });

                self.memory_view(&mut ui[1]);
            });
        });
    }
}

impl Default for App {
    fn default() -> Self {
        Self {
            program: String::new(),
            cpu: DCoreCPU::new(),
        }
    }
}
