extern crate rppal;
extern crate quick_xml;
extern crate clap;

use rppal::gpio::{Gpio, Mode, Level};
use std::path::Path;
use quick_xml::Reader;
use quick_xml::events::Event;
use clap::{Arg, App};

fn main() {

    let pin = 2;
    let mut gpio = Gpio::new().unwrap();
    gpio.set_mode(pin, Mode::Output);
    gpio.set_clear_on_drop(false);

    let matches = App::new("Alarm sync state from XML file")
        .version("1.0")
        .author("Andrei")
        .arg(Arg::with_name("FILENAME"))
        .get_matches();
    let filename = matches.value_of("FILENAME").unwrap();

    let mut reader = match Reader::from_file(Path::new(filename)) {
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
        gpio.write(pin, Level::Low)
    } else {
        gpio.write(pin, Level::High)
    }
}
