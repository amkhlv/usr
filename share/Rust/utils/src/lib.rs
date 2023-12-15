use dirs::home_dir;
use serde::{Deserialize, Serialize};
use serde_dhall::StaticType;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, StaticType)]
pub struct PSQL {
    pub host: String,
    pub port: u64,
    pub sslcert: String,
    pub sslkey: String,
    pub sslrootcert: String,
    pub user: String,
}

pub fn get_psql_conf() -> Result<HashMap<String, PSQL>, serde_dhall::Error> {
    serde_dhall::from_file(
        std::path::Path::new(&home_dir().unwrap()).join("a/Dhall/postgresql.dhall"),
    )
    .parse()
}
pub fn get_system_conf() -> Result<serde_json::Value, serde_dhall::Error> {
    let dhall_string = std::fs::read_to_string(
        std::path::Path::new(&home_dir().unwrap()).join("a/Dhall/system.dhall"),
    )
    .expect("could not read ~/a/Dhall/system.dhall");
    serde_dhall::from_str(&dhall_string).parse()
}
#[macro_export]
macro_rules! declerr {
    ($t:ident,$s:expr) => {
        #[derive(Debug,Clone)]
        struct $t;
        impl fmt::Display for $t {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, $s)
            }
        }
        impl error::Error for $t {}
    };
    ($t:ident($($x:tt)*),$s:expr,$($n:tt)*) => {
        #[derive(Debug,Clone)]
        struct $t($($x)*);
        impl fmt::Display for $t {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, $s, $(self.$n)*)
            }
        }
        impl error::Error for $t {}
    }
}
