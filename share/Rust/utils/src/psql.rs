use clap::{Parser,IntoApp};
use std::process::{Command,Stdio};
use std::collections::HashMap;
use clap_complete::{generate, shells::Bash};
use std::io;
use serde::{Serialize, Deserialize};
use serde_dhall::StaticType;
use dirs::home_dir;


#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
#[clap(author, 
       version, 
       about = "
        interaction with PostgreSQL
        ",
       long_about = None)]
struct Args {
    /// Specify database id
    #[clap(short, long, value_name="DATABASE")]
    base: String,

    /// Commands to execute, if not opens shell
    #[clap(short, long, value_name="COMMAND")]
    commands: Vec<String>,

    /// Generate bash completion
    #[clap(long)]
    completion: bool,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, StaticType)]
struct DataBase {
    host: String,
    port: u64,
    sslcert: String,
    sslkey: String,
    sslrootcert: String,
    user: String
}

fn get_conf() -> Result<HashMap<String,DataBase>, serde_dhall::Error> {
    serde_dhall::from_file(std::path::Path::new(&home_dir().unwrap()).join("a/Dhall/postgresql.dhall")).parse()
}
fn main() -> Result<(), Box<dyn std::error::Error>> {

    let clops = Args::parse();
    if clops.completion {
        generate(Bash, &mut Args::into_app(), "amkhlv-psql", &mut io::stdout());
        return Ok(())
    }
    let conf = get_conf()?;
    let base = conf.get(&clops.base).unwrap();
    let mut args = vec![format!("port={} host={} user={} sslcert={} sslkey={} sslrootcert={} sslmode=verify-ca", 
                                base.port, 
                                base.host, 
                                base.user, 
                                base.sslcert, 
                                base.sslkey, 
                                base.sslrootcert
                               ),
                        "--quiet".to_string()
    ];
    for cmd in clops.commands {
        args.push("-c".to_string());
        args.push(cmd);
    };
    let inter = Command::new("psql")
        .args(args)
        .status();
    if inter.is_err() { println!("STATUS: {:?}",inter); }




    Ok(())
}




