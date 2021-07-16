use std::io::{Write,Read};
use std::fs::File;
use std::env;
use std::os::unix::io::{FromRawFd,IntoRawFd,RawFd};
use clap::{App,Arg};
use amkhlv_tty::{setup};

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let matches = App::new("TTY printer")
        .version("1.0")
        .author("Andrei <a.mkhlv@gmail.com>")
        .about("\nsends $1 to TTY on fd3")
        .after_help("call with 3<>/dev/tty...")
        .usage(&(format!("{} <text> 3<> /dev/tty...",&args[0]))[..])
        .arg(Arg::with_name("text")
             .index(1)
             .required(true)
             .help("what to type"))
        .get_matches();

    let fd = 3;

    setup(fd)?;
    let mut t = unsafe { File::from_raw_fd(fd) };  
    let liter = &args[1];
    write!(&mut t, "{}", liter)?;

    Ok(())
}
