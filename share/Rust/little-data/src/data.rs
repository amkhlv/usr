use gtk::prelude::*;
use gtk::{Application, ApplicationWindow};
use gio::prelude::*;
use glib::clone;
use glib::signal::Inhibit;
use std::io::stdout;
use std::process::{Command,Stdio,ExitStatus};
use std::io::{self,Write};
use std::collections::HashMap;
use std::rc::Rc;
use clap::{Parser,IntoApp};
use clap_complete::{generate, shells::Bash};
use json::{self,JsonValue,object::{Object,Iter}};


#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
#[clap(author, 
       version, 
       about = "
        showing data from a JSON file
        ",
       long_about = None)]
struct Args {
    /// JSON file
    #[clap(short, long, value_name="JSON_FILE")]
    json: String,

    /// Generate bash completion
    #[clap(long)]
    completion: bool,
}

fn xsel(x: String) -> io::Result<ExitStatus> {
    let mut com = Command::new("xsel").args(&["-i"]).stdin(Stdio::piped()).spawn().unwrap();
    let com_in = com.stdin.as_mut().unwrap();
    com_in.write_all(x.as_bytes()).unwrap();
    drop(com_in);
    com.wait()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let clops = Args::parse();
    if clops.completion {
        generate(Bash, &mut Args::into_app(), "amkhlv-data", &mut stdout());
        return Ok(());
    }
    let app = Application::builder()
        .application_id("amkhlv.Data")
        .build();

    let json_string = std::fs::read_to_string(clops.json)?;
    let json_parsed = json::parse(&json_string)?;
    let json = Rc::new(json_parsed);


    app.connect_activate(move |app| {
        // We create the main window.
        let win = ApplicationWindow::builder()
            .application(app)
            .title("Little Data")
            .build();
        let json = json.clone();
        let grid = gtk::Grid::new();

        win.connect_key_press_event(clone!(@weak app => @default-return Inhibit(false), move |_w,e| {
            let keyval = e.keyval(); 
            if *keyval == gdk_sys::GDK_KEY_Escape as u32 {
                app.quit();
            }
            return Inhibit(true);
        }));

        let mut row = 0;
        match &*json {
            JsonValue::Object(o) => {
                o.iter().for_each(|(k,v)| {
                    println!("{:?}",k);
                    if let JsonValue::Short(x) = v {
                        let btn = gtk::Button::new();
                        let lbl = gtk::Label::new(Some(k));
                        btn.add(&lbl);
                        let xx = x.clone();
                        btn.connect_clicked(move |_button| {
                            xsel(String::from(xx)).expect("xsel did not work");
                        }); 
                        grid.attach(&btn,0,row,1,1);
                    }
                    if let JsonValue::String(x) = v {
                        let btn = gtk::Button::new();
                        let lbl = gtk::Label::new(Some(k));
                        btn.add(&lbl);
                        let xx = x.clone();
                        btn.connect_clicked(move |_button| {
                            xsel(xx.clone()).expect("xsel did not work");
                        }); 
                        grid.attach(&btn,0,row,1,1);
                    };
                    if let JsonValue::Object(p) = v {
                        let lbl = gtk::Label::new(Some(k));
                        grid.attach(&lbl,0,row,1,1);
                        let hbox = gtk::Box::new(gtk::Orientation::Horizontal,1);
                        p.iter().for_each(|(key,value)| {
                            if let JsonValue::Short(x1) = value {
                                let btn = gtk::Button::new();
                                let l = gtk::Label::new(Some(key));
                                btn.add(&l);
                                let xx1 = x1.clone();
                                btn.connect_clicked(move |_button| {
                                    xsel(String::from(xx1)).expect("xsel did not work");
                                }); 
                                hbox.add(&btn);
                            }
                            if let JsonValue::String(x1) = value {
                                let btn = gtk::Button::new();
                                let l = gtk::Label::new(Some(key));
                                btn.add(&l);
                                let xx1 = x1.clone();
                                btn.connect_clicked(move |_button| {
                                    xsel(xx1.clone()).expect("xsel did not work");
                                }); 
                                hbox.add(&btn);
                            };
                        });
                        grid.attach(&hbox,1,row,1,1);
                    };
                    row = row + 1;
                })
            }
            _ => panic!()
        };
        win.add(&grid);
        

        // Don't forget to make all widgets visible.
        win.show_all();
    });

    app.run_with_args::<String>(&[]);
    Ok(())
}
