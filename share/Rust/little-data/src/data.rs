use gtk::prelude::*;
use gtk::{Application, ApplicationWindow};
use glib::clone;
use glib::signal::Inhibit;
use std::io::stdout;
use std::process::{Command,Stdio,ExitStatus};
use std::io::{self,Write};
use std::rc::Rc;
use clap::{Parser,IntoApp};
use clap_complete::{generate, shells::Bash};
use serde_json::{self,Value};
use serde_yaml;
use serde_dhall;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
#[clap(author, 
       version, 
       about = "
        showing data from a JSON, YAML or Dhall file
        ",
       long_about = None)]
struct Args {
    /// JSON file
    #[clap(short, long, value_name="JSON_FILE")]
    json: Option<String>,

    /// YAML file
    #[clap(short, long, value_name="YAML_FILE")]
    yaml: Option<String>,

    /// Dhall file
    #[clap(short, long, value_name="YAML_FILE")]
    dhall: Option<String>,

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

fn btn(txt:&str, class:&str, sel:String) -> gtk::Button {
    let btn = gtk::Button::new();
    btn.style_context().add_class(&format!("amkhlv-littledata-button-{}",class));
    let l = gtk::Label::new(Some(txt));
    l.style_context().add_class(&format!("amkhlv-littledata-label-{}",class));
    btn.add(&l);
    btn.connect_clicked(move |_button| {
        xsel(sel.clone()).expect("xsel did not work");
    }); 
    btn
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

    let json = if let Some(j) = clops.json { 
        let json_string = std::fs::read_to_string(j).unwrap();
        let json_parsed = serde_json::from_str(&json_string).unwrap();
        Rc::new(json_parsed)
    } else {
            match (clops.yaml, clops.dhall) {
                (Some(y),_) => {
                    let yaml_string = std::fs::read_to_string(y).unwrap();
                    let json_value: serde_json::Value = serde_yaml::from_str(&yaml_string).unwrap();
                    Rc::new(json_value)
                }
                (None, Some(dhall)) => {
                    let dhall_string = std::fs::read_to_string(dhall).unwrap();
                    let json_value: serde_json::Value = serde_dhall::from_str(&dhall_string).parse().unwrap();
                    Rc::new(json_value)
                }
                (None, None) => { panic!("either JSON or YAML or Dhall file is required"); }
            }
    };

    app.connect_activate(move |app| {
        // We create the main window.
        let win = ApplicationWindow::builder()
            .application(app)
            .title("Little Data")
            .build();
        let json = json.clone();
        let grid = gtk::Grid::new();
        win.style_context().add_class("amkhlv-littledata-mainwin");

        win.connect_key_press_event(clone!(@weak app => @default-return Inhibit(false), move |_w,e| {
            let keyval = e.keyval(); 
            if *keyval == gdk_sys::GDK_KEY_Escape as u32 {
                app.quit();
            }
            return Inhibit(true);
        }));
        let mut row = 0;
        match &*json {
            Value::Object(o) => {
                o.iter().for_each(|(k,v)| {
                    if let Value::String(x) = v {
                        grid.attach(&btn(k,"string",x.clone()),0,row,1,1);
                    };
                    if let Value::Number(n) = v {
                        grid.attach(&btn(k,"number",format!("{}",n)),0,row,1,1);
                    }
                    if let Value::Object(oo) = v {
                        let lbl = gtk::Label::new(Some(k));
                        grid.attach(&lbl,0,row,1,1);
                        let hbox = gtk::Box::new(gtk::Orientation::Horizontal,1);
                        oo.iter().for_each(|(k2,v2)| {
                            if let Value::String(x2) = v2 {
                                hbox.add(&btn(k2,"string",x2.clone()));
                            };
                            if let Value::Number(n2) = v2 {
                                hbox.add(&btn(k2,"number",format!("{}",n2)));
                            };
                            if let Value::Object(ooo) = v2 {
                                let cbox = gtk::ComboBoxText::new();
                                let cblabel = gtk::Label::new(Some(k2));
                                cblabel.style_context().add_class(&format!("amkhlv-littledata-label-cbox"));
                                cbox.set_child(Some(&cblabel));
                                for k3 in ooo.keys() {
                                        if let Value::String(x3) = ooo.get(k3).unwrap() {
                                            cbox.append_text(x3);
                                        }
                                        if let Value::Number(n2) = ooo.get(k3).unwrap() {
                                            cbox.append_text(&format!("{}",n2));
                                        };
                                }
                                cbox.connect_changed(|combo| {
                                    combo.active_iter().map(|itr| {
                                        let model = combo.model();
                                        model.map(|m| {
                                            //println!("{:?}",m.value(&itr,0).get::<String>());
                                            xsel(m.value(&itr,0).get::<String>().unwrap()).expect("xsel did not work");
                                        });
                                    });
                                });
                                hbox.add(&cbox);
                            }
                        });
                        grid.attach(&hbox,1,row,1,1);
                    };
                    row = row + 1;
                })
            }
            _ => panic!()
        };
        win.add(&grid);
        win.show_all();
    });
    app.run_with_args::<String>(&[]);
    Ok(())
}
