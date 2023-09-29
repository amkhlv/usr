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
    let clipboard = Rc::new(RefCell::new(screen.clipboard()));

    let nx_entry_label = Label::new(Some(" Nx:"));
    let nx_entry = Entry::new();
    nx_entry.set_max_width_chars(2);
    nx_entry.set_text("6");

    let ny_entry_label = Label::new(Some(" Ny:"));
    let ny_entry = Entry::new();
    ny_entry.set_max_width_chars(2);
    ny_entry.set_text("5");

    let button_grid = Button::builder()
        .label("grid")
        .margin_top(12)
        .margin_bottom(12)
        .margin_start(12)
        .margin_end(12)
        .build();
    button_grid.set_css_classes(&["steam-down"]);

    let vbox = Box::new(gtk::Orientation::Vertical, 5);
    // grid:
    let hbox_grid = Box::new(gtk::Orientation::Horizontal, 5);
    hbox_grid.append(&nx_entry_label);
    hbox_grid.append(&nx_entry);
    hbox_grid.append(&ny_entry_label);
    hbox_grid.append(&ny_entry);
    hbox_grid.append(&button_grid);
    vbox.append(&hbox_grid);
    let window = ApplicationWindow::builder()
        .application(app)
        .title("svg helper")
        .child(&vbox)
        .build();

    let nxentry = Rc::new(RefCell::new(nx_entry));
    let nyentry = Rc::new(RefCell::new(ny_entry));
    let clipboard1 = clipboard.clone();
    button_grid.connect_clicked(clone!(@weak app => move |button| {
        let nxentry1 = nxentry.clone();
        let nyentry1 = nyentry.clone();
        let clipboard = clipboard1.clone();
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

        clipboard.borrow_mut().set_text(&document.to_string());
        app.quit();
    }));
    // hyperlink:
    let hbox_href = Box::new(gtk::Orientation::Horizontal, 5);
    let label_href_txt = Label::new(Some(" Text:"));
    let en_href_txt = Entry::new();
    let label_href_url = Label::new(Some(" URL:"));
    let en_href_url = Entry::new();
    let btn_href = Button::builder().label("hyperlink").build();
    hbox_href.append(&label_href_txt);
    hbox_href.append(&en_href_txt);
    hbox_href.append(&label_href_url);
    hbox_href.append(&en_href_url);
    vbox.append(&hbox_href);
    let rc_en_href_txt = Rc::new(RefCell::new(en_href_txt));
    let rc_en_href_url = Rc::new(RefCell::new(en_href_url));
    let clipboard1 = clipboard.clone();
    btn_href.connect_clicked(clone!(@weak app => move |button| {
        let clipboard = clipboard1.clone();
        let rc_en_href_txt = rc_en_href_txt.clone();
        let rc_en_href_url = rc_en_href_url.clone();
        let link = svg::node::element::Link::new()
            .add(svg::node::element::Text::new()
                 .add(svg::node::element::TSpan::new()
                      .set("style", "text-decoration:underline;fill:blue")
                      .add(svg::node::Text::new(rc_en_href_txt.borrow().buffer().text().to_string()))))
            .set("xlink:href",rc_en_href_url.borrow().buffer().text().to_string());
        let document = Document::new().add(link);
        clipboard.borrow_mut().set_text(&document.to_string());
        app.quit();
    }));
    hbox_href.append(&btn_href);
    // annotation:
    let hbox_annot = Box::new(gtk::Orientation::Horizontal, 5);
    let en_annot = Entry::new();
    hbox_annot.append(&en_annot);
    let clipboard1 = clipboard.clone();
    en_annot.connect_activate(clone!(@weak app => move |entry| {
        let clipboard = clipboard1.clone();
        let a = entry.buffer().text().to_string();
        clipboard.borrow_mut().set_text(&format!("
<svg xmlns=\"http://www.w3.org/2000/svg\">
    <ellipse
       style=\"fill:#ff8c00;stroke-width:0.8\"
       cx=\"8\"
       cy=\"-20\"
       rx=\"4\"
       ry=\"4\"><desc>{}</desc></ellipse>
    <text
       xml:space=\"preserve\"
       style=\"fill:green;font-size:26.6667px;line-height:33.7036px;font-family:'DejaVu Sans';-inkscape-font-specification:'DejaVu Sans, Normal';letter-spacing:0px;word-spacing:0px;writing-mode:lr-tb\"
       x=\"15\"
       y=\"-12\"><tspan sodipodi:role=\"line\" x=\"20\" y=\"-32\">{}</tspan></text>
</svg>", a, a
        ));
        app.quit();
    }));
    vbox.append(&hbox_annot);
    // Present window
    window.present();
}
