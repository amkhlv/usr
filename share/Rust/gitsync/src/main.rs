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

fn main() {
    let clops = Clops::parse();
    let mut siv = cursive::termion();
    siv.add_global_callback(Key::Esc, |s| s.quit());
    let cancelled: Rc<RefCell<bool>> = Rc::new(RefCell::new(true));
    let cancelled1 = cancelled.clone();

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
        for line in String::from_utf8(output.stdout)
            .unwrap()
            .lines()
            .map(|line| line.trim())
        {
            let mut split = line.split_whitespace();
            if let Some(x) = split.next() {
                if x.contains("M") || x.contains("?") {
                    let mut hbox = LinearLayout::new(Orientation::Horizontal);
                    hbox.add_child(Checkbox::new());
                    hbox.add_child(TextView::new(&format!("{}", split.next().unwrap())));
                    list = list.child(if x.contains("M") { "M" } else { "?" }, hbox);
                }
            }
        }
        mainwin.add_child(list.with_name("modified"));
        mainwin.add_child(TextView::new(" mod↑"));
        mainwin.add_child(Button::new("push", move |s| {
            cancelled1.replace(false);
            s.call_on_name("modified", |xs: &mut ListView| {
                //let mut cmd = "git add ".to_owned();
                for x in xs.children() {
                    match x {
                        ListChild::Row(_name, v) => {
                            if let Some(ll) = v.downcast_ref::<LinearLayout>() {
                                if let Some(mcbox) = ll.get_child(0) {
                                    if let Some(cbox) = mcbox.downcast_ref::<Checkbox>() {
                                        if cbox.is_checked() {
                                            if let Some(mtv) = ll.get_child(1) {
                                                if let Some(tv) = mtv.downcast_ref::<TextView>() {
                                                    let mut orig = cmd.borrow_mut();
                                                    orig.push(tv.get_content().source().to_owned());
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        _ => {}
                    }
                }
            });
            s.quit();
        }));

        mainwin
    });

    siv.run();
    let was_cancelled = cancelled.borrow();
    std::cell::Ref::map(was_cancelled, |x| {
        if *x {
            println!("user cancelled");
            std::process::exit(0);
        }
        x
    });
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
