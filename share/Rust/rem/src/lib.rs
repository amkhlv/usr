use cursive::views::{LinearLayout, TextView};
use cursive::CbSink;
use dirs::home_dir;
use rustls::{Certificate, PrivateKey};
use rustls_pemfile::{read_one, Item};
use serde::Deserialize;
use std::fs::File;
use std::io::BufReader;
use std::process::Command;
use tokio_postgres_rustls::MakeRustlsConnect;

#[derive(Debug, Clone, Deserialize)]
pub struct PSQL1 {
    pub host: String,
    pub port: u16,
    pub user: String,
    pub dbname: String,
    pub sslcert: String,
    pub sslkey: String,
    pub sslrootcert: String,
}

pub fn get_db_conf_ncl(
    path: impl Into<std::ffi::OsString>,
) -> Result<PSQL1, nickel_lang_core::deserialize::EvalOrDeserError> {
    nickel_lang_core::deserialize::from_path(path)
}
#[derive(Deserialize)]
pub struct DB {
    dbnick: String,
}
pub fn get_tls(psql: PSQL1) -> Result<MakeRustlsConnect, Box<dyn std::error::Error>> {
    let rootpem_file = File::open(&psql.sslrootcert).expect(&format!(
        "unable to open root cert file: {}",
        &psql.sslrootcert
    ));
    let mut rootpem_buf = BufReader::new(rootpem_file);
    let rootder = read_one(&mut rootpem_buf).expect("unable to parse root cert");
    let rootcert = match rootder {
        Some(Item::X509Certificate(v)) => v,
        _ => panic!("failing to extract root certificate"),
    };
    let certpem_file: File = File::open(&psql.sslcert).expect(&format!(
        "unable to open certificate file: {}",
        &psql.sslcert
    ));
    let mut certpem_buf = BufReader::new(certpem_file);
    let certder = read_one(&mut certpem_buf).expect("unable to parse ssl cert");
    let cert = match certder {
        Some(Item::X509Certificate(v)) => v,
        _ => panic!("failing to extract client certificate"),
    };
    let keypem_file =
        File::open(&psql.sslkey).expect(&format!("unable to open key file: {}", &psql.sslkey));
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
    }))
    .expect("unable to alert");
}

pub fn vibrate(msg: &str) -> () {
    //Command::new("termux-vibrate").args(["-d","50"]).spawn();
    Command::new("notify-send").args(["-t", "1", msg]).spawn();
}
