extern crate rppal;
extern crate clap;

use rppal::gpio::{Gpio, Mode, Level};
use rppal::system::DeviceInfo;
use clap::{Arg, App};

// The GPIO module uses BCM pin numbering. BCM GPIO 18 is tied to physical pin 12.

fn main() {
    let matches = App::new("Raspberry Pi GPIO helper")
                          .version("1.0")
                          .author("Andrei")
                          .about("Switches relays")
                          .arg(Arg::with_name("PIN")
                               .short("p")
                               .long("pin")
                               .value_name("P")
                               .help("Which pin")
                               .required(true))
                          .arg(Arg::with_name("LOW")
                               .short("l")
                               .long("--low")
                               .help("set LOW"))
                          .get_matches();
    let pin = matches.value_of("PIN").unwrap().parse::<u8>().unwrap();
    
    let device_info = DeviceInfo::new().unwrap();
    println!("Model: {} (SoC: {})", device_info.model(), device_info.soc());

    let mut gpio = Gpio::new().unwrap();
    gpio.set_mode(pin, Mode::Output);
    gpio.set_clear_on_drop(false);

    // Blink an LED attached to the pin on and off
    if matches.is_present("LOW") {
        gpio.write(pin, Level::Low);
    } else {
        gpio.write(pin, Level::High);
    }
}
