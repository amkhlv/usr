extern crate yaml_rust;
extern crate pancurses;
extern crate dirs;
extern crate linked_hash_map;

use std::path::{Path,PathBuf};
use yaml_rust::{YamlLoader,Yaml};
use pancurses::{initscr, endwin, Window, Input, init_pair, start_color, has_colors, noecho, A_BOLD, curs_set, COLOR_PAIR, COLOR_BLACK, COLOR_GREEN, A_NORMAL};
use std::{error,fmt};
use dirs::home_dir;
use linked_hash_map::LinkedHashMap;
use std::io::{BufWriter, Write};
use std::process::{Command, Stdio};

#[derive(Debug, Clone)]
struct NoHomeDir;
impl fmt::Display for NoHomeDir {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "could not find HOME dir")
    }
}
impl error::Error for NoHomeDir {}

#[derive(Debug, Clone)]
struct NoKey;
impl fmt::Display for NoKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "could not find key")
    }
}
impl error::Error for NoKey {}

#[derive(Debug, Clone)]
struct BadKey(String);
impl fmt::Display for BadKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BadKey: {}", self.0)
    }
}
impl error::Error for BadKey {}

#[derive(Debug, Clone)]
struct BadSchema(String);
impl fmt::Display for BadSchema {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "BadSchema: {}", self.0)
    }
}
impl error::Error for BadSchema {}

#[derive(Debug, Clone)]
struct NoVal;
impl fmt::Display for NoVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "could not find key")
    }
}
impl error::Error for NoVal {}

fn step(w: Window, h: &LinkedHashMap<Yaml,Yaml>) ->  Result<(), Box<dyn std::error::Error>> {
    let width = w.get_max_x();
    let mut i = 0u8;
    for k in h.keys() {
        match &k {
            Yaml::String(sec) => { 
                w.mv(3 + 3*(i as i32/3), (i as i32 % 3) * (std::cmp::min(width, 99) / 3));
                w.attrset(COLOR_PAIR(1) | A_BOLD);
                w.addstr(format!("{} ", (i + 97) as char));
                w.attrset(A_NORMAL);
                w.addstr(&sec) ; 
                i += 1;
            },
            _ => { return Err(Box::new(BadKey(format!("{:?}", k)))) ; },
        }
    };
    match w.getch() {
        Some(Input::Character(c)) => {
            w.clear();
            let ks = &mut (h.keys().skip(((c as u8) - 97) as usize));
            let key = match ks.next() {
                Some(key) => key,
                None => { return Err(Box::new(NoKey)); }
            };
            match h.get(key) {
                Some(Yaml::Hash(h)) => step(w,h),
                Some(Yaml::String(s)) => {
                    let a = vec!["-i"];
                    let p = Command::new("xsel").args(&a).stdin(Stdio::piped()).spawn().unwrap();
                    let mut stdin = p.stdin.unwrap();
                    let mut writer = BufWriter::new(&mut stdin);
                    writer.write(s.as_bytes()).unwrap();
                    endwin();
                    println!("{}\n\n\n", s);
                    Ok(())
                },
                Some(y) => { return Err(Box::new(BadSchema(format!("{:?}",y)))); },
                None => { return Err(Box::new(NoVal)) ; },
            }
        },
        _ => Ok(())
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let home: Option<PathBuf> = home_dir() ;
    let home = match home {
        Some(h) => h,
        None => { return Err(Box::new(NoHomeDir)); }
    };

    let a_str = std::fs::read_to_string(Path::join(Path::new(&home), "a.yaml"))?;
    let a : Vec<Yaml> = YamlLoader::load_from_str(&a_str)?;
    println!("{:?}", a[0]);

    let window = initscr();
    window.refresh();
    window.keypad(true);
    noecho();
    curs_set(0);
    if has_colors() {
        start_color();
        init_pair(1, COLOR_GREEN, COLOR_BLACK);
    };

    let result = match &a[0] {
        Yaml::Hash(htop) => step(window,htop),
        _ => Ok(())
    };
    endwin();
    result

}
