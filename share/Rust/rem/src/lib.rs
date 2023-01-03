use serde::Deserialize;
use dirs::home_dir;
use std::process::Command;
use cursive::CbSink;
use cursive::views::{LinearLayout,TextView};
use std::fs::File;
use std::io::BufReader;
use rustls::{PrivateKey,Certificate};
use rustls_pemfile::{Item,read_one};
use tokio_postgres_rustls::MakeRustlsConnect;

use amkhlv::{get_psql_conf,PSQL};

#[derive(Deserialize)]
pub struct DB {
    dbnick: String
}
pub fn get_db_conf() -> Result<PSQL, Box<dyn std::error::Error>>  {
    let whichdb: DB = serde_dhall::from_file(home_dir().unwrap().join(".config").join("amkhlv").join("rem").join("config.dhall")).parse().unwrap();
    let psql_conf = get_psql_conf()?;
    let psql: &PSQL = psql_conf.get(&whichdb.dbnick).unwrap();
    Ok(psql.clone())
}
pub fn get_tls() ->  Result<MakeRustlsConnect, Box<dyn std::error::Error>> {
    let psql = get_db_conf()?;
    let rootpem_file = File::open(&psql.sslrootcert).expect(&format!("unable to open root cert file: {}",&psql.sslrootcert));
    let mut rootpem_buf = BufReader::new(rootpem_file);
    let rootder = read_one(&mut rootpem_buf).expect("unable to parse root cert");
    let rootcert = match rootder {
        Some(Item::X509Certificate(v)) => v,
        _ => panic!("failing to extract root certificate")
    };
    let certpem_file: File = File::open(&psql.sslcert).expect(&format!("unable to open certificate file: {}",&psql.sslcert));
    let mut certpem_buf = BufReader::new(certpem_file);
    let certder = read_one(&mut certpem_buf).expect("unable to parse ssl cert");
    let cert = match certder {
        Some(Item::X509Certificate(v)) => v,
        _ => panic!("failing to extract client certificate")
    };
    let keypem_file = File::open(&psql.sslkey).expect(&format!("unable to open key file: {}",&psql.sslkey));
    let mut keypem_buf = BufReader::new(keypem_file);
    let keyder = read_one(&mut keypem_buf).expect("unable to parse ssl key");
    let key = match keyder {
        Some(Item::PKCS8Key(v)) => v,
        x => {
            println!("failing to extract PKCS8 key, getting instead: {:?}", x);
            panic!()
        }
    };
    let mut root: rustls::RootCertStore = rustls::RootCertStore::empty();
    root.add(&Certificate(rootcert))?;
    let config = rustls::ClientConfig::builder()
    .with_safe_defaults()
    .with_root_certificates(root)
    .with_single_cert(vec![Certificate(cert)], PrivateKey(key))?;
    Ok(tokio_postgres_rustls::MakeRustlsConnect::new(config))
}

pub fn wait(sink: &CbSink, msg: &str) {
    let msg = msg.to_owned();
    sink.send(Box::new(|s| {
        s.pop_layer();
        let vb = LinearLayout::vertical().child(TextView::new(msg));
        s.add_layer(vb);
    })).expect("unable to alert");
}

pub fn vibrate(msg: &str) -> () {
    //Command::new("termux-vibrate").args(["-d","50"]).spawn();
    Command::new("notify-send").args(["-t","1",msg]).spawn();
}
