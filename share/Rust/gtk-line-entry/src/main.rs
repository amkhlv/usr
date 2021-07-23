#[macro_use]
extern crate gdk;
extern crate gdk_sys;
extern crate gtk;
extern crate glib;
extern crate gio;
extern crate clap;
extern crate dirs;
extern crate yaml_rust;
extern crate shellexpand;

use clap::{Arg,App};
use gtk::prelude::*;
use gio::prelude::*;
use glib::clone;
use std::path::Path;
use dirs::home_dir;
use yaml_rust::{YamlLoader,Yaml};
use gtk::{Application, ApplicationWindow, Entry};
use std::process::{Command,Stdio};
use std::io::{BufWriter,Write};
use std::rc::Rc;

fn main() {

    let application = Application::new(
        Some("com.github.gtk-rs.examples.basic"),
        Default::default(),
        ).expect("failed to initialize GTK application");

    application.connect_activate(|app| {
        let clops = App::new("line entry")
            .author("Andrei Mikhailov")
            .about("Line entry")
            .arg(Arg::with_name("conf")
                 .value_name("conf")
                 .takes_value(true)
                 .help("YAML configuration file")
                 .short("c"))
            .get_matches();
        let default_config_file = Path::join(Path::new(&home_dir().unwrap()), ".config/amkhlv/line-input/id.yaml");
        let config_file = clops.value_of("conf").unwrap_or(default_config_file.to_str().unwrap());
        let configs = YamlLoader::load_from_str(&std::fs::read_to_string(Path::new(config_file)).unwrap()).unwrap();
        let conf: &Yaml = &configs[0];
        let enter: Rc<String> = Rc::new(String::from(conf["enter"].as_str().unwrap()));
        let buttons: Rc<&Vec<Yaml>> = Rc::new(conf["buttons"].as_vec().unwrap());
        let window = ApplicationWindow::new(app);
        window.set_title(&conf["title"].as_str().unwrap());
        window.set_default_size(conf["width"].as_i64().unwrap() as i32, conf["height"].as_i64().unwrap() as i32);
        window.connect_key_press_event(clone!(@weak app => @default-return Inhibit(false), move |_w,e| {
            let keyval = e.get_keyval(); 
            let _keystate = e.get_state();
            if *keyval == gdk_sys::GDK_KEY_Escape as u32 {
                app.quit();
                return Inhibit(true);
            } else { return Inhibit(false); }
        }));
        let vbox = gtk::Box::new(gtk::Orientation::Vertical,10);
        let entry = Entry::new();
        entry.connect_activate(clone!(@weak entry, @weak app => move |_| {
            let p = Command::new(String::from(shellexpand::tilde(&*enter))).stdin(Stdio::piped()).spawn().unwrap();
            let mut stdin = p.stdin.unwrap();
            let mut writer = BufWriter::new(&mut stdin);
            writer.write(entry.get_text().as_bytes()).unwrap();
            app.quit();
        }));
        vbox.add(&entry);
        let hbox = gtk::Box::new(gtk::Orientation::Horizontal,10);
        for button in *buttons {
            let btn = gtk::Button::new();
            let lbl = gtk::Label::new(Some(button["label"].as_str().unwrap()));
            btn.add(&lbl);
            hbox.add(&btn);
            let action = Rc::new(String::from(button["callback"].as_str().unwrap()));
            btn.connect_clicked(clone!(@weak entry, @weak app => move |_| {
                let p = Command::new(String::from(shellexpand::tilde(&*action))).stdin(Stdio::piped()).spawn().unwrap();
                let mut stdin = p.stdin.unwrap();
                let mut writer = BufWriter::new(&mut stdin);
                writer.write(entry.get_text().as_bytes()).unwrap();
                app.quit();
            }));

        };
        vbox.add(&hbox);
        window.add(&vbox);

        window.show_all();
    });
    application.run(&[]);
}

