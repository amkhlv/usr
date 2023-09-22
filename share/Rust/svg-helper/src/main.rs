use clap::{IntoApp, Parser};
use gdk4::{prelude::DisplayExt, Clipboard, Display};
use glib::clone;
use gtk::prelude::*;
use gtk::{glib, Application, ApplicationWindow, Box, Button, CssProvider, Entry, Label};
use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;
use svg::node::element::path::Data;
use svg::node::element::Path;
use svg::Document;

const APP_ID: &str = "com.andreimikhailov.svg-helper";

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
#[clap(
    author,
    version,
    about = "
        puts SVG prefabs into clipboard
        "
)]
struct Args {}

fn main() -> glib::ExitCode {
    // Create a new application
    let app = Application::builder().application_id(APP_ID).build();

    let clops = Args::parse();
    // Connect to "activate" signal of `app`
    app.connect_activate(move |a| build_ui(a));

    // Run the application
    let empty: Vec<String> = vec![];
    let exit_code = app.run_with_args(&empty);
    exit_code
}

fn build_ui(app: &Application) {
    let mut css = home::home_dir().expect("could not locate home dir");
    css.push(".config");
    css.push("amkhlv");
    css.push("svg-helper.css");
    let provider = CssProvider::new();
    println!("using css: {}", css.to_str().unwrap());
    provider.load_from_path(css.to_str().unwrap());
    let screen = gdk4::Display::default().unwrap();
    gtk::style_context_add_provider_for_display(&screen, &provider, 799);
    let clipboard = screen.clipboard();

    let nx_entry_label = Label::new(Some(" Nx:"));
    let nx_entry = Entry::new();
    nx_entry.set_max_width_chars(2);
    nx_entry.set_text("6");

    let ny_entry_label = Label::new(Some(" Ny:"));
    let ny_entry = Entry::new();
    ny_entry.set_max_width_chars(2);
    ny_entry.set_text("5");

    let button = Button::builder()
        .label("grid")
        .margin_top(12)
        .margin_bottom(12)
        .margin_start(12)
        .margin_end(12)
        .build();
    button.set_css_classes(&["steam-down"]);

    let vbox = Box::new(gtk::Orientation::Vertical, 5);

    let hbox = Box::new(gtk::Orientation::Horizontal, 5);
    hbox.append(&nx_entry_label);
    hbox.append(&nx_entry);
    hbox.append(&ny_entry_label);
    hbox.append(&ny_entry);
    hbox.append(&button);
    vbox.append(&hbox);
    let window = ApplicationWindow::builder()
        .application(app)
        .title("svg helper")
        .child(&vbox)
        .build();

    let nxentry = Rc::new(RefCell::new(nx_entry));
    let nyentry = Rc::new(RefCell::new(ny_entry));
    button.connect_clicked(clone!(@weak app => move |button| {
        let nxentry1 = nxentry.clone();
        let nyentry1 = nyentry.clone();
        let nx = nxentry1.borrow().buffer().text().parse::<u32>().unwrap();
        let ny = nyentry1.borrow().buffer().text().parse::<u32>().unwrap();
        let marg = 10;
        let stepx = 100;
        let stepy = 70;

        let mut dat = Data::new();
        for x in 0..(nx + 1) {
            dat = dat
                .move_to((marg + stepx * x, marg))
                .line_by((0, ny * stepy));
        }
        for y in 0..(ny + 1) {
            dat = dat
                .move_to((marg, marg + stepy * y))
                .line_by((nx * stepx, 0));
        }

        let path = Path::new()
            .set("fill", "none")
            .set("stroke", "black")
            .set("stroke-width", 1)
            .set("d", dat);

        let document = Document::new()
            .set(
                "viewBox",
                (0, 0, 2 * marg + stepx * nx, 2 * marg + stepy * ny),
            )
            .add(path);

        clipboard.set_text(&document.to_string());
        app.quit();
    }));

    // Present window
    window.present();
}
