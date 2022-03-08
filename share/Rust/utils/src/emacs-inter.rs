use clap::{Parser,IntoApp};
use std::process::{Command,Stdio};
use clap_complete::{generate, shells::Bash};
use std::io;
use std::io::BufReader;
use std::io::prelude::*;
use regex::Regex;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
#[clap(author, 
       version, 
       about = "
        interaction with Emacs
        ",
       long_about = None)]
struct Args {
    /// Specify SOCKET
    #[clap(short, long, value_name="SOCKET")]
    socket: Option<String>,

    /// Open file with socket, requires SOCKET
    #[clap(short, long, value_name="FILE")]
    open: Option<String>,

    /// Kill socket
    #[clap(short, long)]
    kill: bool,
    
    /// Revive socket
    #[clap(short, long)]
    revive: bool,

    /// List Emacs sockets
    #[clap(short,long)]
    list: bool,

    /// Generate bash completion
    #[clap(long)]
    completion: bool,
}
fn main() -> Result<(), Box<dyn std::error::Error>> {

    let clops = Args::parse();
    
    if clops.completion {
        generate(Bash, &mut Args::into_app(), "amkhlv-emacs-inter", &mut io::stdout());
        return Ok(())
    }

    if clops.list {
        let re = Regex::new(r"/run/user/[0-9]+/emacs/(?P<name>[^[:space:]]+)").unwrap();
        let lsof = Command::new("lsof")
            .args(["-U", "+E"])
            .stdout(Stdio::piped())
            .spawn()?;
        let br = BufReader::new(lsof.stdout.unwrap());
        for rln in br.lines() {
            if let Ok(ln) = rln {
                if let Some(m) = re.captures_iter(&ln).into_iter().next() {
                    println!("{:?}", &m["name"]);
                }
            }
        };
        return Ok(());
    }
    if let Some(f) = clops.open {
        if let Some(s)  = clops.socket {
            Command::new("emacsclient")
                .args(["-s", &s, &f])
                .spawn()?;
            return Ok(());
        } else {
            panic!("should have provided the socket name");
        }
    }
    if clops.kill {
        if let Some(s)  = clops.socket {
            println!("Killing socket {}", s);
            let killer = Command::new("emacsclient")
                .args(["-s", &s, "-e", "(kill-emacs)"])
                .status();
            println!("{:?}",killer);
            return Ok(());
        } else {
            panic!("should have provided the socket name");
        }
    }
    if clops.revive {
        if let Some(s)  = clops.socket {
            println!("Creating window for socket {}", s);
            let revive = Command::new("emacsclient")
                .args(["-c", "-s", &s])
                .status();
            println!("{:?}",revive);
            return Ok(());
        } else {
            panic!("should have provided the socket name");
        }
    }
    Ok(())
}




