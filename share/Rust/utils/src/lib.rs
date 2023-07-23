use std::collections::HashMap;
use serde::{Serialize, Deserialize};
use serde_dhall::StaticType;
use dirs::home_dir;

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, StaticType)]
pub struct PSQL {
    pub host: String,
    pub port: u64,
    pub sslcert: String,
    pub sslkey: String,
    pub sslrootcert: String,
    pub user: String
}

pub fn get_psql_conf() -> Result<HashMap<String,PSQL>, serde_dhall::Error> {
    serde_dhall::from_file(std::path::Path::new(&home_dir().unwrap()).join("a/Dhall/postgresql.dhall")).parse()
}
pub fn get_system_conf() -> Result<serde_json::Value, serde_dhall::Error> {
    let dhall_string = std::fs::read_to_string(
        std::path::Path::new(&home_dir().unwrap()).join("a/Dhall/system.dhall")
    ).expect("could not read ~/a/Dhall/system.dhall");
    serde_dhall::from_str(&dhall_string).parse()
}
