use gtk::prelude::*;
use gtk::{glib, CssProvider, Application, ApplicationWindow,Box,Button,Entry,Label};
use gdk4::Display;
use std::rc::Rc;
use std::cell::RefCell;
use std::io::Write;
use serialport::{TTYPort, SerialPort, SerialPortBuilder};
use clap::{Parser,IntoApp};
use std::thread::sleep;
use std::time::Duration;

const APP_ID: &str = "com.andreimikhailov.steamer";

const ON: char = 'A';
const OFF: char = 'B';

#[derive(Clone,Copy)]
struct CurrentRunSeries {
    steaming: u16,
    resting: u16,
    n: u16
}

enum CurrentRun {
    Steaming(u16),
    Resting(u16)
}

enum State {
    Running(u16, CurrentRunSeries, CurrentRun),
    Idling
}

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
#[clap(author, 
       version, 
       about = "
        serial tty example
        ")]
struct Args {
    /// device
    #[clap(short, long)]
    device: String,
}


fn main() -> glib::ExitCode {
    // Create a new application
    let app = Application::builder().application_id(APP_ID).build();

    let clops = Args::parse();
    // Connect to "activate" signal of `app`
    let mut port = serialport::new(clops.device, 9600).timeout(Duration::from_millis(1000)).open_native().expect("failed to open TTY");
    sleep(Duration::from_millis(750));
    port.write_all(&[ON as u8]).expect("could not write to port");
    port.flush().expect("could not flush");
    sleep(Duration::from_millis(750));
    port.write_all(&[OFF as u8]).expect("could not write to port");
    port.flush().expect("could not flush");
    let port0 = Rc::new(RefCell::new(port));
    let port1 = port0.clone();
    let port2 = port0.clone();
    app.connect_activate(move |a| { build_ui(a,port1.clone()) });


    // Run the application
    let empty: Vec<String> = vec![];
    let exit_code = app.run_with_args(&empty);
    std::mem::drop(port2);
    exit_code
}

fn build_ui(app: &Application, port: Rc<RefCell<TTYPort>>) {

    let mut css = home::home_dir().expect("could not locate home dir");
    css.push(".config");
    css.push("amkhlv");
    css.push("steamer.css");
    let provider = CssProvider::new();
    println!("using css: {}",css.to_str().unwrap());
    provider.load_from_path(css.to_str().unwrap());
    let screen = gdk4::Display::default().unwrap();
    gtk::style_context_add_provider_for_display(&screen, &provider, 799);

    let state = State::Idling;
    let state = Rc::new(RefCell::new(state));

    let runs_entry_label = Label::new(Some(" runs:"));
    let runs_entry = Entry::new();
    let up_time_entry_label = Label::new(Some(" up:"));
    let up_time_entry = Entry::new();
    let down_time_entry_label = Label::new(Some(" down:"));
    let down_time_entry = Entry::new();
    // Create a button with label and margins
    let button = Button::builder()
        .label("start")
        .margin_top(12)
        .margin_bottom(12)
        .margin_start(12)
        .margin_end(12)
        .build();
    button.set_css_classes(&["steam-down"]);
    let runs_label = Label::new(Some("remaining runs: --"));
    let timer_label = Label::new(Some("idle"));

    let vbox = Box::new(gtk::Orientation::Vertical, 5);

    let hbox = Box::new(gtk::Orientation::Horizontal, 5);
    hbox.append(&runs_entry_label);
    hbox.append(&runs_entry);
    hbox.append(&up_time_entry_label);
    hbox.append(&up_time_entry);
    hbox.append(&down_time_entry_label);
    hbox.append(&down_time_entry);
    hbox.append(&button);
    vbox.append(&hbox);
    let hbox1 = Box::new(gtk::Orientation::Horizontal, 5);
    hbox1.append(&runs_label);
    hbox1.append(&timer_label);
    vbox.append(&hbox1);
    let window = ApplicationWindow::builder()
        .application(app)
        .title("steamer")
        .child(&vbox)
        .build();
    let ren = Rc::new(RefCell::new(runs_entry));
    let uen = Rc::new(RefCell::new(up_time_entry));
    let den = Rc::new(RefCell::new(down_time_entry));
    let rl = Rc::new(RefCell::new(runs_label));
    let tl = Rc::new(RefCell::new(timer_label));
    let rl1 = rl.clone();
    let tl1 = tl.clone();
    let ren1 = ren.clone();
    let uen1 = uen.clone();
    let den1 = den.clone();
    let state1 = state.clone();
    let port1 = port.clone();
    button.connect_clicked(move |button| {
        let state = state1.clone();
        let ren = ren.clone();
        let uen = uen.clone();
        let den = den.clone();
        let rl = rl.clone();
        let tl = tl.clone();
        let port2 = port1.clone();
        state.replace_with(move | st | { match st {
            State::Running(x, crs, cr) => {
                button.set_label("run");
                button.set_css_classes(&["steam-down"]);
                rl.borrow_mut().set_text(" -- ");
                tl.borrow_mut().set_text(" -- ");
                port2.borrow_mut().write_all(&[OFF as u8]).expect("could not write to port");
                port2.borrow_mut().flush().expect("could not flush");
                State::Idling
            }
            State::Idling => {
                button.set_label("stop");
                button.set_css_classes(&["steam-up"]);
                let n = ren.borrow().buffer().text().parse::<u16>().unwrap();
                let steaming = uen.borrow().buffer().text().parse::<u16>().unwrap();
                let resting = den.borrow().buffer().text().parse::<u16>().unwrap();
                rl.borrow_mut().set_text(&format!(" started {} runs ", n));
                tl.borrow_mut().set_text(&format!(" steaming-{} ", steaming));
                port2.borrow_mut().write_all(&[ON as u8]).expect("could not write to port");
                port2.borrow_mut().flush().expect("could not flush");
                State::Running(n, CurrentRunSeries { n, steaming, resting  }, CurrentRun::Steaming(steaming))
            }
        }});
    });

    let state1 = state.clone();
    let port1 = port.clone();
    let button1 = Rc::new(RefCell::new(button));
    glib::source::timeout_add_local(
        std::time::Duration::from_millis(1000), 
        move || { 
            let port2 = port1.clone();
            let state = state1.clone();
            let rl1 = rl1.clone();
            let tl1 = tl1.clone();
            let button = button1.clone();
            state.replace_with(| st | { match st {
                State::Running(x, crs, CurrentRun::Steaming(0)) => { 
                    tl1.borrow_mut().set_text(&format!(" resting-{} ", crs.resting));
                    port2.borrow_mut().write_all(&[OFF as u8]).expect("could not write to port");
                    port2.borrow_mut().flush().expect("could not flush");
                    State::Running(*x, *crs, CurrentRun::Resting(crs.resting))
                }
                State::Running(x, crs, CurrentRun::Resting(0)) => {
                    if *x == 0 { 
                        tl1.borrow_mut().set_text(&format!(" -- "));
                        rl1.borrow_mut().set_text(&format!(" -- "));
                        button.borrow_mut().set_label("start");
                        button.borrow_mut().set_css_classes(&["steam-down"]);
                        port2.borrow_mut().write_all(&[OFF as u8]).expect("could not write to port");
                        port2.borrow_mut().flush().expect("could not flush");
                        State::Idling 
                    } else { 
                        tl1.borrow_mut().set_text(&format!(" steaming-{} ", crs.steaming));
                        rl1.borrow_mut().set_text(&format!(" remaining {} runs ", *x - 1));
                        port2.borrow_mut().write_all(&[ON as u8]).expect("could not write to port");
                        port2.borrow_mut().flush().expect("could not flush");
                        State::Running(*x - 1, *crs, CurrentRun::Steaming(crs.steaming))
                    }
                }
                State::Running(x, crs, CurrentRun::Steaming(n)) => {
                    tl1.borrow_mut().set_text(&format!(" steaming-{} ", *n - 1));
                    State::Running(*x, *crs, CurrentRun::Steaming(*n-1))
                }
                State::Running(x, crs, CurrentRun::Resting(n)) => {
                    tl1.borrow_mut().set_text(&format!(" resting-{} ", *n - 1));
                    port2.borrow_mut().write_all(&[OFF as u8]).expect("could not write to port");
                    port2.borrow_mut().flush().expect("could not flush");
                    State::Running(*x, *crs, CurrentRun::Resting(*n-1)) 
                }
                State::Idling => {
                    State::Idling
                }
            }});
            glib::ControlFlow::Continue 
        }
        );


    // Present window
    window.present();
}
