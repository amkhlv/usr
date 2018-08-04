#![feature(plugin)]
#![plugin(rocket_codegen)]

extern crate rocket;
extern crate rppal;
extern crate quick_xml;

use rppal::gpio::{Gpio, Mode, Level};
use std::sync::mpsc;
use std::sync::mpsc::SyncSender;
use std::thread;
use std::fs;
use rocket::State;
use rocket::response::content::Html;
use rocket::response::Redirect;
use std::path::Path;
use quick_xml::Reader;
use quick_xml::events::Event;

#[get("/")]
fn index(sender: State<SyncSender<bool>>) -> Html<&'static str> {
    let mut reader = match Reader::from_file(Path::new("./alarm.xml")) {
        Ok(r) => r,
        Err(_) => panic!("Error reading state XML file")
    };
    let mut buf = Vec::new();
    let mut is_armed = false;
    loop {
        match reader.read_event(&mut buf) {
            Ok(Event::Start(ref e)) => {
                match e.name() {
                    b"armed" => { println!("-- is armed") ; let a = &mut is_armed; *a = true; },
                    _ => (),
                }
            },
            Ok(Event::Eof) => break, // exits the loop when reaching end of file
            _ => (), // There are several other `Event`s we do not consider here
        }
    }
    if is_armed { 
            sender.send(true).unwrap();
            Html(
                r#"<html>
                <head>
                <meta http-equiv="refresh" content="300"></meta>
                </head>
                <body>
                <tag style="font-size:100px;color:white;background-color:red">Armado</tag>
                <form action="/off" method="post"> <input style="font-size:110px;" type="submit" value="Desarmar"></input></form>
                </body>
                </html>"#
                )
        } else { 
            sender.send(false).unwrap();
            Html(
                r#"<html>
                <head>
                <meta http-equiv="refresh" content="300"></meta>
                </head>
                <body>
                <tag style="font-size:100px;color:white;background-color:green">Desarmado</tag>
                <form action="/on" method="post"> <input style="font-size:110px;" type="submit" value="Armar"></input></form>
                </body>
                </html>"#
                )
            }
}
#[post("/on")]
fn activate(sender: State<SyncSender<bool>>) -> Redirect {
    sender.send(true).unwrap();
    fs::write("alarm.xml", "<armed></armed>");
    Redirect::to("/")
}
#[post("/off")]
fn desactivate(sender: State<SyncSender<bool>>) -> Redirect {
    sender.send(false).unwrap();
    fs::write("alarm.xml", "<disarmed></disarmed>");
    Redirect::to("/")
}

fn main() {
    let (sender, receiver) = mpsc::sync_channel::<bool>(1);

    thread::spawn(move || {
        let pin = 2;
        let mut gpio = Gpio::new().unwrap();
        gpio.set_mode(pin, Mode::Output);
        gpio.set_clear_on_drop(false);
        loop {
            if receiver.recv().unwrap() {
                gpio.write(pin, Level::Low);
            } else {
                gpio.write(pin, Level::High);
            }
        }
    });

    rocket::ignite()
        .manage(sender)
        .mount("/", routes![index,activate,desactivate])
        .launch();
}
