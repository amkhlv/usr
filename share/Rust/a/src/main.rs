extern crate dirs;
extern crate linked_hash_map;
extern crate pancurses;

use amkhlv::declerr;
use clap::{App, Arg};
use dirs::home_dir;
use pancurses::{
    curs_set, endwin, has_colors, init_pair, initscr, noecho, start_color, Input, Window, A_BOLD,
    A_NORMAL, COLOR_BLACK, COLOR_GREEN, COLOR_PAIR,
};
use serde::{Deserialize, Serialize};
use serde_dhall;
use std::collections::HashMap;
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::{error, fmt};

declerr![NoHomeDir, "could not find HOME dir"];
declerr![NoKey(char), "could not find key {}", 0];
declerr![BadKey(String), "bad key: >>>{}<<<", 0];
declerr![BadSchema(String), "BadSchema: {}", 0];
declerr![NoVal(String), "no such key: {}", 0];

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
enum AData {
    Leaf(String),
    Branch(HashMap<String, AData>),
}

fn step(w: Window, h: &AData) -> Result<(), Box<dyn std::error::Error>> {
    let width = w.get_max_x();
    let mut i = 0u8;
    let hh = {
        if let AData::Branch(x) = h {
            x
        } else {
            panic!("internal error");
        }
    };
    for k in hh.keys() {
        w.mv(
            3 + 3 * (i as i32 / 3),
            (i as i32 % 3) * (std::cmp::min(width, 99) / 3),
        );
        w.attrset(COLOR_PAIR(1) | A_BOLD);
        w.addstr(format!("{} ", (i + 97) as char));
        w.attrset(A_NORMAL);
        w.addstr(&k);
        i += 1;
    }
    match w.getch() {
        Some(Input::Character(c)) => {
            w.clear();
            let ks = &mut (hh.keys().skip(((c as u8) - 97) as usize));
            let key = match ks.next() {
                Some(key) => key,
                None => {
                    return Err(Box::new(NoKey(c)));
                }
            };
            match hh.get(key) {
                Some(br @ AData::Branch(_)) => step(w, br),
                Some(AData::Leaf(s)) => {
                    let a = vec!["-i"];
                    let p = Command::new("xsel")
                        .args(&a)
                        .stdin(Stdio::piped())
                        .spawn()
                        .unwrap();
                    let mut stdin = p.stdin.unwrap();
                    let mut writer = BufWriter::new(&mut stdin);
                    writer.write(s.as_bytes()).unwrap();
                    endwin();
                    println!("{}\n\n\n", s);
                    let home: Option<PathBuf> = home_dir();
                    let home = match home {
                        Some(h) => h,
                        None => {
                            return Err(Box::new(NoHomeDir));
                        }
                    };
                    std::fs::write(Path::join(Path::new(&home), ".local/var/run.sh"), s).unwrap();
                    Ok(())
                }
                None => {
                    return Err(Box::new(NoVal(String::from(key))));
                }
            }
        }
        _ => Ok(()),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let matches = App::new("Explore Dhall a-list")
        .version("1.0")
        .author("Andrei <a.mkhlv@gmail.com>")
        .about("a:\n  b:\n    - c\n    ...\n  ...\n...")
        .arg(Arg::with_name("dhallfile").multiple(true))
        .get_matches();
    let home: Option<PathBuf> = home_dir();
    let home = match home {
        Some(h) => h,
        None => {
            return Err(Box::new(NoHomeDir));
        }
    };
    let iterator = matches.values_of("dhallfile");

    let a_str = match iterator {
        None => std::fs::read_to_string(Path::join(Path::new(&home), "a.dhall"))?,
        Some(mut it) => std::fs::read_to_string(it.next().unwrap())?,
    };

    let a: AData = serde_dhall::from_str(&a_str).parse()?;

    let window = initscr();
    window.refresh();
    window.keypad(false);
    noecho();
    curs_set(0);
    if has_colors() {
        start_color();
        init_pair(1, COLOR_GREEN, COLOR_BLACK);
    };

    let result = step(window, &a);

    endwin();
    result
}
