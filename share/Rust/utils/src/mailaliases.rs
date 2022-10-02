use std::collections::HashMap;
use std::io;
use serde::{Serialize, Deserialize};
use serde_dhall::StaticType;
use dirs::home_dir;



#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, StaticType)]
struct Person {
    nick: String,
    name: String,
    email: String
}

fn get_dhall() -> Result<Vec<Person>, serde_dhall::Error> {
    serde_dhall::from_file(std::path::Path::new(&home_dir().unwrap()).join("a/Dhall/emails.dhall")).parse()
}
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let dhall = get_dhall()?;
    for person in dhall {
        println!("alias {} \"{}\" <{}>",person.nick, person.name, person.email);
    }
    Ok(())
}

