use std::cell::{Cell, RefCell};
use std::{env, io};

use clap::{Parser, Subcommand};
use cursive::direction::Orientation;
use cursive::event::Key;
use cursive::view::Nameable;
use cursive::views::{
    Button, Checkbox, Dialog, EditView, LinearLayout, ListChild, ListView, PaddedView, Panel,
    TextArea, TextView,
};
use cursive::CbSink;
use std::fs::OpenOptions;
use std::io::prelude::*;
use std::process::Command;
use std::rc::Rc;

#[derive(Parser)]
struct Clops {
    /// branch name
    #[clap(short, long, default_value = "main")]
    branch: String,
}

fn checkbox_name(i: u16) -> String {
    format!("checkbox-{}", i)
}

fn textview_name(i: u16) -> String {
    format!("textview-{}", i)
}

fn main() {
    let clops = Clops::parse();
    let mut siv = cursive::termion();
    siv.add_global_callback(Key::Esc, |s| s.quit());
    let esced: Rc<Cell<bool>> = Rc::new(Cell::new(true));
    let esced1 = esced.clone();

    let command: Rc<RefCell<Vec<String>>> = Rc::new(RefCell::new(Vec::new()));
    let cmd = command.clone();

    siv.add_layer({
        let mut mainwin = LinearLayout::new(Orientation::Vertical);

        let mut list = ListView::new();

        let output = Command::new("git")
            .arg("status")
            .arg("--porcelain")
            .output()
            .expect("could not run git");
        let mut i = 0;
        for line in String::from_utf8(output.stdout)
            .unwrap()
            .lines()
            .map(|line| line.trim())
        {
            let mut split = line.split_whitespace();
            if let Some(x) = split.next() {
                if x.contains("M") || x.contains("?") {
                    let mut hbox = LinearLayout::new(Orientation::Horizontal);
                    hbox.add_child(Checkbox::new().with_name(checkbox_name(i)));
                    hbox.add_child(
                        TextView::new(&format!("{}", split.next().unwrap()))
                            .with_name(textview_name(i)),
                    );
                    i = i + 1;
                    list = list.child(if x.contains("M") { "M" } else { "?" }, hbox);
                }
            }
        }
        mainwin.add_child(list.with_name("modified"));
        mainwin.add_child(TextView::new(" mod↑"));
        mainwin.add_child(Button::new("push", move |s| {
            esced1.replace(false);
            for j in 0..i {
                if s.call_on_name(&checkbox_name(j), |w: &mut Checkbox| w.is_checked())
                    .unwrap()
                {
                    let mut cmd_ref = cmd.borrow_mut();
                    cmd_ref.push(
                        s.call_on_name(&textview_name(j), |tv: &mut TextView| {
                            tv.get_content().source().to_owned()
                        })
                        .unwrap(),
                    )
                }
            }
            s.quit();
        }));
        mainwin
    });

    siv.run();

    let esced: bool = esced.get();
    if esced {
        println!("user cancelled");
        std::process::exit(0);
    }
    let filelist = command.borrow();
    let mut gitadd = Command::new("git")
        .arg("add")
        .args(filelist.clone())
        .spawn()
        .expect("could not run git add");
    if let Ok(exit_status) = gitadd.wait() {
        if exit_status.success() {
            println!("enter commit message:");
            let stdin = io::stdin();
            if let Some(msg) = stdin.lock().lines().next() {
                let mut gitcommit = Command::new("git")
                    .arg("commit")
                    .arg("-m")
                    .arg(msg.expect("error reading user input"))
                    .spawn()
                    .expect("could not run git commit");
                if let Ok(exit_status_1) = gitcommit.wait() {
                    if exit_status_1.success() {
                        println!("pushing...");
                        let mut gitpush = Command::new("git")
                            .arg("push")
                            .arg("origin")
                            .arg(clops.branch)
                            .spawn()
                            .expect("could not run git push");
                        if let Ok(exit_status_2) = gitpush.wait() {
                            if exit_status_2.success() {
                                println!("DONE");
                            } else {
                                println!("ERROR: {:?}", exit_status_2);
                            }
                        } else {
                            println!("ERROR");
                        }
                    } else {
                        println!("ERROR committing: {:?}", exit_status_1);
                    }
                }
            };
        } else {
            println!("ERROR: {:?}", exit_status);
        }
    } else {
        println!("ERROR running git add");
    };
}
