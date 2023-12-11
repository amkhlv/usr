use serde::{Deserialize, Serialize};
use serde_dhall;
use serde_json;
use serde_yaml;
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
enum AData {
    Leaf(String),
    Rec(HashMap<String, AData>),
}

fn main() {
    let yaml_string = std::fs::read_to_string(std::path::Path::new("/home/andrei/a.yaml"))
        .expect("could not read ~/a.yaml");
    let yaml = serde_yaml::from_str::<AData>(&yaml_string).unwrap();
    let dhall = serde_dhall::serialize(&yaml);
    println!("{}", dhall.to_string().unwrap());
}
