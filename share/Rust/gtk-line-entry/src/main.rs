#[macro_use]
extern crate gtk;
extern crate glib;
extern crate gio;

use std::env;
use gtk::prelude::*;
use gio::prelude::*;
use glib::clone;

use gtk::{Application, ApplicationWindow, Entry};

fn main() {
    let application = Application::new(
        Some("com.github.gtk-rs.examples.basic"),
        Default::default(),
    ).expect("failed to initialize GTK application");

    application.connect_activate(|app| {
        let args: Vec<String> = env::args().collect();
        let window = ApplicationWindow::new(app);
        window.set_title(&args[1]);
        window.set_default_size(1000, 70);

        let entry = Entry::new();
        entry.connect_activate(clone!(@weak entry, @weak app => move |_| {
            println!("{}", entry.get_text());
            app.quit();
        }));
        window.add(&entry);

        window.show_all();
    });

    application.run(&[]);
}

