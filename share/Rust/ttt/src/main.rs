use std::env;

use chrono::{DateTime, Local, TimeDelta};
use cursive::view::Nameable;
use cursive::views::{
    Button, Dialog, EditView, LinearLayout, PaddedView, Panel, TextArea, TextView,
};
use cursive::CbSink;
use std::fs::OpenOptions;
use std::io::prelude::*;
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    macro_rules! log_file {
        () => {
            "ttt.csv"
        };
    }
    let mut file = OpenOptions::new()
        .write(true)
        .append(true)
        .open(log_file![])
        .expect(concat!("-- no file ", log_file![], " found...\n"));
    let args = env::args();
    let activity_name = args.skip(1).collect::<Vec<String>>().join(" ");
    let start_time = Local::now();
    let start_mins = start_time.timestamp_millis() / 60000;
    let mut siv = cursive::termion();
    let cb_sink = siv.cb_sink().clone();
    siv.set_fps(30);

    siv.add_layer(
        Dialog::around(TextView::new("0 min").with_name("elapsed"))
            .title(activity_name.clone())
            .button("Finished", |s| s.quit()),
    );

    tokio::spawn(async move {
        loop {
            sleep(Duration::from_millis(10000)).await;
            let now_mins = Local::now().timestamp_millis() / 60000;
            cb_sink
                .send(Box::new(move |s| {
                    s.call_on_name("elapsed", |view: &mut TextView| {
                        view.set_content(format!("{} min", now_mins - start_mins));
                    })
                    .expect("unable to set label")
                }))
                .expect("unable to communicate with cb_sink");
        }
    });
    siv.run();
    let finish_time = Local::now();
    let diff: TimeDelta = finish_time - start_time;
    let mins: i64 = diff.num_minutes();
    if let Err(e) = writeln!(
        file,
        "{}, {:>3}, {}",
        start_time.format("%Y-%m-%d %H:%M"),
        mins,
        activity_name
    ) {
        eprintln!(
            concat!("Couldn't write to ", log_file![], " error is: {}"),
            e
        );
    }
}
