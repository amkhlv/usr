extern crate gtk;
extern crate glib;
extern crate gdk;
extern crate gio;
extern crate yaml_rust;
extern crate dirs;
extern crate gdk_sys;
extern crate linked_hash_map;


use std::env;
use std::cell::{RefCell,RefMut};
use std::rc::Rc;
use gtk::prelude::*;
use gio::prelude::*;
use glib::clone;
use glib::signal::Inhibit;
use dirs::home_dir;
use std::{error,fmt};
use std::collections::HashMap;
use std::path::{Path,PathBuf};
use yaml_rust::{YamlLoader,Yaml};
use linked_hash_map::{Iter, LinkedHashMap};
use gtk::{Application, ApplicationWindow, Grid, Label, Button, Entry};

#[derive(Debug, Clone)]
struct NoHomeDir;
impl fmt::Display for NoHomeDir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "could not find HOME dir")
    }
}
impl error::Error for NoHomeDir {}

#[derive(Debug, Clone)]
struct NoKey;
impl fmt::Display for NoKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "could not find key")
    }
}
impl error::Error for NoKey {}

#[derive(Debug, Clone)]
struct BadKey(String);
impl fmt::Display for BadKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BadKey: {}", self.0)
    }
}
impl error::Error for BadKey {}

#[derive(Debug, Clone)]
struct BadSchema(String);
impl fmt::Display for BadSchema {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BadSchema: {}", self.0)
    }
}
impl error::Error for BadSchema {}

#[derive(Debug, Clone)]
struct NoVal;
impl fmt::Display for NoVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "could not find key")
    }
}
impl error::Error for NoVal {}

// https://users.rust-lang.org/t/how-to-get-static-lifetime/5552/8
#[derive(Debug, Clone)]
struct Command { data: Rc<String> }
impl Command {
    fn new(data: Rc<String>) -> Self {
        Command { data: data }
    }
}
#[derive(Debug, Clone)]
struct Charhints { data: Rc<HashMap<u8,String>> }
impl Charhints {
    fn new(data: Rc<HashMap<u8,String>>) -> Self {
        Charhints { data: data }
    }
}

fn run_command(x:&String) {
    std::process::Command::new(Path::new(&home_dir().unwrap()).join(".config/amkhlv/things/bin/").join(x)).spawn().unwrap();
}

fn get_command_source(x:&String) -> String {
    return std::fs::read_to_string(Path::new(&home_dir().unwrap()).join(".config/amkhlv/things/bin/").join(x)).unwrap();
}

fn main()  {

    let application = Application::new(
        Some("com.andreimikhailov.things"),
        Default::default(),
        ).expect("failed to initialize GTK application");


    application.connect_activate(|app| {
        let o_str = std::fs::read_to_string(Path::join(Path::new(&home_dir().unwrap()), ".config/amkhlv/things/things.yaml")).unwrap();
        let tophash = {
            let vyaml = YamlLoader::load_from_str(&o_str).unwrap();
            let mut hm: HashMap<String, Vec<HashMap<String,String>>> = HashMap::new();
            match &vyaml[0] {
                Yaml::Hash(x) => {
                    for kv in x.iter() {
                        match (kv.0, kv.1) {
                            (Yaml::String(ttl), Yaml::Array(v)) => {
                                let mut items: Vec<HashMap<String,String>> = Vec::new();
                                for vv in v {
                                    match vv {
                                        Yaml::Hash(h) => {
                                            let mut pair: HashMap<String,String> = HashMap::new();
                                            for u in h.iter() {
                                                match (u.0, u.1) {
                                                    (Yaml::String(a), Yaml::String(b)) => {
                                                        pair.insert(a.clone(), b.clone());
                                                    }
                                                    _ => ()
                                                }
                                            };
                                            items.push(pair);
                                        },
                                        _ => ()
                                    }
                                };
                                hm.insert(ttl.clone(),items);
                            },
                            _ => ()
                        }
                    }
                }
                _ => ()
            }
            hm
        };

        let css = Path::join(Path::new(&home_dir().unwrap()), ".config/amkhlv/things/style.css");
        let provider = gtk::CssProvider::new();
        match css.to_str() {
            Some(x) => { 
                match provider.load_from_path(x) {
                    Ok(_) => (),
                    Err(x) => { println!("ERROR: {:?}", x); }
                }
            }
            None => { println!("ERROR: CSS file not found") ; }
        };
        let screen = gdk::Screen::get_default();
        match screen {
            Some(scr) => { gtk::StyleContext::add_provider_for_screen(&scr, &provider, 799); }
            _ => ()
        };
        let window = ApplicationWindow::new(app);
        window.set_title("Things");
        let grid = Grid::new();
        grid.set_row_spacing(10);
        grid.set_column_spacing(10);
        grid.get_style_context().add_class("things-grid");
        window.add(&grid);
        let mut charhints : HashMap<u8, String> = HashMap::new();
        let mut i = 0u8;
        let mut j = 0;
        for (subj, items) in &mut tophash.iter() {
            if j % 3 == 0 { grid.insert_row(j as i32 / 3); }
            let vbox = gtk::Box::new(gtk::Orientation::Vertical, 3);
            vbox.get_style_context().add_class(&format!("things-vbox-{}",subj));
            grid.attach(&vbox, j as i32 % 3, j as i32 / 3, 1, 1);
            vbox.add(&Label::new(Some(subj)));
            for item in items {
                let hbox = gtk::Box::new(gtk::Orientation::Horizontal, 3);
                let image = gtk::Image::new();
                image.set_from_file(
                    match item.get("pic") {
                        Some(ttl) => ttl,
                        _ => "/usr/share/icons/gnome/24x24/apps/utilities-terminal.png",
                    });
                let charlabel = gtk::Label::new(Some(&format!("{}",(i+97) as char)));
                charlabel.get_style_context().add_class("things-item-charhint-label");
                let button = gtk::Button::new();
                let cmd = match item.get("script") {
                    Some(s) => s,
                    _  => "true"
                };
                &charhints.insert(i+97,cmd.to_owned());
                let c = Rc::new(Command::new(Rc::new(cmd.to_owned())));
                let c1 = c.clone();
                button.connect_clicked(clone!(@weak app => move |_| {
                    println!("Running: {}",&c1.data);
                    run_command(&c1.data);
                    app.quit();
                }));
                let label = gtk::Label::new(
                    match item.get("description") {
                        Some(ttl) => Some(ttl),
                        _ => Some("ERROR"),
                    });
                label.get_style_context().add_class("things-item-label");
                button.add(&label);
                button.get_style_context().add_class("things-item-button");
                button.set_tooltip_text(Some(&get_command_source(&c.data)));
                hbox.add(&image);
                hbox.add(&charlabel);
                hbox.add(&button);
                vbox.add(&hbox);
                i += 1;
            }
            j += 1;
        }
        println!("{:?}", charhints);
        let hints = Rc::new(Charhints::new(Rc::new(charhints)));
        let hints1 = hints.clone();
        window.connect_key_press_event(clone!(@weak app => @default-return Inhibit(false), move |_w,e| {
            let keyval = e.get_keyval(); 
            let keystate = e.get_state();
            if *keyval == gdk_sys::GDK_KEY_Escape as u32 {
                println!("Exiting...");
                app.quit();
                return Inhibit(true);
            }
            println!("key pressed: {} / {:?}", *keyval, keystate);
            let a = (format!("{}",*keyval)).parse::<u8>();
            println!("{:?} -> {:?}", &a, &hints1);
            match a {
                Ok(aa) => {
                    if let Some(s) = &hints1.data.get(&aa) {
                        println!("Running: {}",&s);
                        run_command(s);
                        app.quit();
                        return Inhibit(true);
                    } else {
                        return Inhibit(false);
                    }
                },
                _ => { return Inhibit(false); }
            }
        }));
        window.show_all();
    });
    application.run(&[]);
}
