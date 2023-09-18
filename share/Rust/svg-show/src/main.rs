use clap::Parser;
use gtk::prelude::*;
use gtk::{glib, Application, ApplicationWindow, CssProvider};
use notify::{
    event::{AccessKind, AccessMode, EventKind},
    Event, RecursiveMode, Result, Watcher,
};
use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::{Arc, Mutex};

const APP_ID: &str = "com.andreimikhailov.svg-show";

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
#[clap(
    author,
    version,
    about = "
        serial tty example
        "
)]
struct Args {
    /// directory to watch, if any
    #[clap(short, long)]
    directory: Option<String>,

    /// initial file, if any
    #[clap(short, long)]
    file: Option<String>,

    #[clap(short, long)]
    width: Option<i32>,

    #[clap(short, long)]
    height: Option<i32>,
}

struct Situation {
    count: i32,
    path: Option<PathBuf>,
}

fn main() -> glib::ExitCode {
    // Create a new application
    let app = Application::builder().application_id(APP_ID).build();

    let situation: Arc<Mutex<Situation>> = Arc::new(Mutex::new(Situation {
        count: 0,
        path: None,
    }));
    let clops = Args::parse();
    // Connect to "activate" signal of `app`
    let maybe_dir = clops.directory.clone();

    let situation0 = situation.clone();
    app.connect_activate(move |a| {
        let situation1 = situation0.clone();
        build_ui(a, &clops, situation1)
    });

    let situation1 = situation.clone();
    let mut watcher = notify::recommended_watcher(move |res: Result<Event>| match res {
        Ok(event) => match event.kind {
            EventKind::Access(AccessKind::Close(AccessMode::Write)) => {
                let mut s = situation1.lock().unwrap();
                let p = event.paths[0].clone();
                let mime = mime_guess::from_path(p).first_or_octet_stream();
                if mime.type_() == mime::IMAGE {
                    s.path = Some(event.paths[0].clone());
                    s.count += 1;
                } else {
                    println!("--- MIME type = >>>{}<<<", mime.type_());
                }
            }
            _ => {}
        },
        Err(e) => println!("watch error: {:?}", e),
    })
    .unwrap();
    if let Some(dir) = maybe_dir {
        watcher
            .watch(Path::new(&dir), RecursiveMode::Recursive)
            .unwrap();
    }

    // Run the application
    let empty: Vec<String> = vec![];
    app.run_with_args(&empty)
}

fn build_ui(app: &Application, clops: &Args, counter: Arc<Mutex<Situation>>) {
    let update_counter: Rc<RefCell<i32>> = Rc::new(RefCell::new(0));
    let mut css = home::home_dir().expect("could not locate home dir");
    css.push(".config");
    css.push("amkhlv");
    css.push("svg-show.css");
    let provider = CssProvider::new();
    println!("using css: {}", css.to_str().unwrap());
    provider.load_from_path(css.to_str().unwrap());
    let screen = gdk4::Display::default().unwrap();
    gtk::style_context_add_provider_for_display(&screen, &provider, 799);
    let window = ApplicationWindow::builder()
        .application(app)
        .title("SVG")
        .build();
    let window = Rc::new(window);
    //let drawing_area = gtk::DrawingArea::new();
    //window.set_child(Some(&drawing_area));
    if let Some(filepath) = clops.file.clone() {
        //let pixbuf = gdk_pixbuf::Pixbuf::from_file(&Path::new(&filepath)).unwrap();
        let image = gtk::Image::from_file(Path::new(&filepath));
        window.set_child(Some(&image));
    }
    let counter1 = counter.clone();
    let update_counter1 = update_counter.clone();
    window.set_default_size(clops.width.unwrap_or(1024), clops.height.unwrap_or(800));
    let window1 = window.clone();
    glib::source::timeout_add_local(std::time::Duration::from_millis(250), move || {
        let s = counter1.lock().unwrap();
        //println!("counter is {}, update_counter is {}", s.count, update_counter1.borrow());
        let i = update_counter1.replace(s.count);
        if s.count > i {
            if let Some(p) = s.path.clone() {
                let image = gtk::Image::from_file(p);
                window1.set_child(Some(&image));
            }
        }
        glib::ControlFlow::Continue
    });

    // Present window

    window.present();
}
