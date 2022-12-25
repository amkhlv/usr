use serde::Deserialize;
use dirs::home_dir;
use openssl::ssl::{SslConnector,SslMethod,SslVerifyMode,SslFiletype};
use postgres_openssl::MakeTlsConnector;
use postgres::Client;
use std::path::Path;
use std::process::Command;
use cursive::{Cursive,CursiveRunnable,CbSink};
use cursive::theme::{Theme,BorderStyle,Palette,BaseColor::*,Color::*,PaletteColor::*};
use cursive::views::{EditView,Dialog,LinearLayout,TextView,Button,Panel,PaddedView};
use std::rc::Rc;
use std::cell::RefCell;
use std::sync::mpsc::{Sender, Receiver};
use std::sync::mpsc;
use std::thread::{self, JoinHandle};
use std::future::Future;
use std::fs::File;
use std::io::{self,BufReader};

/*
use rustls::{PrivateKey,Certificate,ClientConfig};
use rustls_pemfile::{Item,read_one};
use tokio_postgres_rustls::MakeRustlsConnect;
use tokio_postgres::Socket;
use tokio_postgres::tls::MakeTlsConnect;
 */

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
pub fn get_db_connector() -> Result<MakeTlsConnector, Box<dyn std::error::Error>>  {
    let psql = get_db_conf()?;
    let mut builder = SslConnector::builder(SslMethod::tls()).expect("unable to create sslconnector builder");
    //let x509 = openssl::x509::X509::from_pem(&std::fs::read(Path::new(&psql.sslrootcert)).unwrap()).unwrap();
    //builder.add_client_ca(&x509).expect("unable to add root cert");
    builder.set_certificate_file(&psql.sslcert,SslFiletype::PEM).expect("unable to load certificate");
    builder.set_private_key_file(&psql.sslkey,SslFiletype::PEM).expect("unable to set key");
    builder.set_ca_file(&psql.sslrootcert).expect("unable to set Root certificate");
    builder.set_verify(SslVerifyMode::PEER);
    let sslconn = builder.build();
    let connector = MakeTlsConnector::new(sslconn);
    Ok(connector)
}
pub fn get_client() -> Result<Client, Box<dyn std::error::Error>> {
    let connector = get_db_connector()?;
    let psql = get_db_conf()?;
    let conn = Client::connect(
        &format!("host={} port={} user={} sslmode=require", &psql.host, &psql.port, &psql.user), 
        connector
        ).expect("failed to create tls postgres connection");
    Ok(conn)
}
/*
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
*/

pub enum LThemes {
    Alert,
    Normal,
    Wait
}
pub fn mksiv(theme: LThemes) -> CursiveRunnable {
    let mut siv = cursive::termion();
    siv.set_fps(10);
    let mut palette = Palette::default();
    match theme {
        LThemes::Alert => { palette.extend(vec![(Background,Light(Red))]); }
        LThemes::Wait  => { palette.extend(vec![(Background,Dark(Yellow))]); }
        LThemes::Normal => { palette.extend(vec![(Background,Light(Yellow))]); }
    }
    siv.set_theme(Theme {shadow: false, borders: BorderStyle::Outset, palette});
    siv
}
pub fn wait() {
    let w = mksiv(LThemes::Wait);
    let mut vbox = LinearLayout::vertical();
    vbox = vbox.child(TextView::new("WAIT..."));
    let rw = Rc::new(RefCell::new(w));
    rw.borrow_mut().add_layer(vbox);
    let sink = rw.borrow_mut().cb_sink().clone();
    thread::spawn(move || {
        std::thread::sleep(std::time::Duration::from_millis(250));
        sink.send(Box::new(|x: &mut Cursive| x.quit()));
    });
    rw.borrow_mut().run();
}

pub struct Waiter {
    tx: Sender<()>,
    rx: Receiver<()>,
    handle: JoinHandle<()>
}
pub fn show_banner(msg: &str) -> Waiter {
    let (tx1, rx1): (Sender<()>, Receiver<()>) = mpsc::channel();
    let (tx2, rx2): (Sender<()>, Receiver<()>) = mpsc::channel();
    let msg1 = msg.to_owned();
    let t = thread::spawn(move ||{
        let mut w0 = mksiv(LThemes::Wait);
        let mut vbox = LinearLayout::vertical();
        vbox = vbox.child(TextView::new(msg1));
        w0.add_layer(vbox);
        let sink : &CbSink = w0.cb_sink();
        let s = sink.clone();
        thread::spawn(move || { 
            rx1.recv().expect("failed recv") ; 
            s.send(Box::new(|x: &mut Cursive| { x.quit(); })).expect("failed send to CbSink"); 
            tx2.send(()).expect("failed send to tx2"); });
        w0.run(); 
    });
    Waiter {tx : tx1, rx : rx2, handle : t}
}
pub fn dismiss(wtr: Waiter) {
    wtr.tx.send(()).expect("could not send quit signal"); 
    wtr.rx.recv().expect("could not receive from tmp win thread"); 
    wtr.handle.join().expect("could not joint tem win thread");
}
pub fn vibrate() -> () {
    Command::new("termux-vibrate").args(["-d","50"]).spawn();
}
