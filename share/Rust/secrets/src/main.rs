use clap::{Parser,IntoApp};
use std::process::{Command,Stdio};
use std::io::{BufReader,BufWriter,Write};
use std::io::prelude::*;
use xmltree::Element;
use cursive::views::{EditView,Dialog,LinearLayout,TextView,TextContent,Button,Panel,PaddedView};
use cursive::CbSink;
use std::rc::Rc;
use tokio::sync::mpsc;
use futures::executor::block_on;
use std::time::Duration;
use tokio::time::{Timeout,timeout,error::Elapsed};
use std::future::Future;
use std::sync::Arc;
use unix_named_pipe::create as create_pipe;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
#[clap(author, 
       version, 
       about = "
        password manager
        "
       )]
struct Args {
    /// file
    #[clap(short, long, value_name="FILE")]
    file: String
}

#[derive(Debug)]
enum Front2Back {
    EnteringPassword(String,String),
    ShowSecretData(String),
    Ready
}
#[derive(Debug)]
enum Back2Front {
}

fn get_sites<'a>(nick: &str, sites: &'a Element) -> Vec<&'a Element> {
    let mut r = Vec::new();
    for site in &sites.get_child("sites").unwrap().children {
        site.as_element().map(|site_elt| {
            site_elt.attributes.get("nick").map(|ni| {
                if ni.contains(nick) {
                    r.push(site_elt);
                }
            })
        });
    }
    println!("number of matches: {}", r.len());
    r
}

fn initial_dialog(sites: Arc<Element>, tx: mpsc::Sender<Front2Back>) -> Dialog {
    Dialog::text(format!("secrets"))
        .title("Secrets")
        .content(EditView::new()
                 .on_submit(move |win, t| {
                     let mut vbox = LinearLayout::vertical();
                     for site in get_sites(t, sites.as_ref()) {
                         let mut hbox = LinearLayout::horizontal();
                         hbox.add_child(TextView::new_with_content(TextContent::new(site.attributes.get("nick").unwrap())));
                         for account in &site.children {
                             let account1 = account.clone();
                             if let Some(acct) = account1.as_element() {
                                 if let Some(login) = acct.attributes.get("login") {
                                     let lgn = String::from(login);
                                     if let Some(pwd) = acct.get_child("password") {
                                         let password = pwd.get_text().unwrap().to_string();
                                         let tx1 = tx.clone();
                                         let tx2 = tx.clone();
                                         hbox.add_child(TextView::new_with_content(TextContent::new(" --- ")));
                                         hbox.add_child(Button::new(login, move |_win| {
                                             block_on(tx1.send(Front2Back::EnteringPassword(
                                                         lgn.clone(),
                                                         password.clone()
                                                         ))).expect("error asking to Wait");
                                         }));
                                         if let Some(secnotes) = acct.get_child("secret_notes") {
                                             if let Some(secret_text) = secnotes.get_text() {
                                                 let secret = secret_text.to_string();
                                                 hbox.add_child(Button::new("*", move |_win | {
                                                     block_on(tx2.send(Front2Back::ShowSecretData(secret.clone()))).expect("could not send command to show data");
                                                 }));
                                             }
                                         };
                                     }}};
                         }
                         vbox.add_child(hbox);
                     }
                     win.pop_layer();
                     win.add_layer(Dialog::around(vbox));
                 }))
}

fn show_secret_data_dialog(x: String, tx: mpsc::Sender<Front2Back>) -> Dialog {
    let tx1 = tx.clone();
     Dialog::around(TextView::new(x)).button("Ok", move |_s| {
         block_on(tx1.send(Front2Back::Ready)).expect("error sending Ready");
     })
}

fn waiting_dialog() -> Dialog {
                     let mut vbox = LinearLayout::vertical();
                     vbox.add_child(TextView::new_with_content(TextContent::new("--WAITING--")));
                     Dialog::around(vbox)
}


#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let clops = Args::parse();
    let decryptor = Command::new("gpg")
        .args(["--decrypt", &clops.file])
        .stdout(Stdio::piped())
        .spawn()?;
    let br  = BufReader::new(decryptor.stdout.unwrap());
    let sites = Element::parse(br).unwrap();
    let sites = Arc::new(sites);

    let (tx_f2b, mut rx_f2b) = mpsc::channel::<Front2Back>(1);

    let mut siv = cursive::termion();
    siv.set_fps(30);
    let sites1 = sites.clone();
    let tx0 = tx_f2b.clone();
    siv.add_layer(initial_dialog(sites1, tx0));
    let cb_sink = siv.cb_sink().clone();


    let sites2 = sites.clone();
    let tx1 = tx_f2b.clone();
    // backend:
    tokio::spawn(async move {
        loop {
            match rx_f2b.recv().await {
                Some(Front2Back::EnteringPassword(login,password)) => {
                    //println!("asked to wait");
                    cb_sink.send(Box::new(move |s| {
                        s.pop_layer();
                        s.add_layer(waiting_dialog());
                    })).expect("failed to update UI to Wait");
                    let tx2 = tx1.clone();
                    let mut kbpipe = home::home_dir().expect("could not obtain home dir");
                    kbpipe.push(".amkhlv-keyboardpipe.fifo");
                    std::fs::remove_file(kbpipe.clone()).expect(&format!("could not remove pipe >>>{:?}<<<",kbpipe));
                    create_pipe(kbpipe.clone(),None).expect(&format!("could not create pipe >>>{:?}<<<",kbpipe));
                    let command = std::fs::read_to_string(kbpipe.as_path()).unwrap();
                    if command.trim() == "go" {
                        let amkbd = Command::new("amkbd").stdin(Stdio::piped()).spawn().expect("cannot run amkbd");
                        let mut bw = BufWriter::new(amkbd.stdin.unwrap());
                        bw.write_all(login.as_bytes()).expect("cannot send login to stding of amkbd");
                    }
                    let command = std::fs::read_to_string(kbpipe.as_path()).unwrap();
                    if command.trim() == "go" {
                        let amkbd = Command::new("amkbd").stdin(Stdio::piped()).spawn().expect("cannot run amkbd");
                        let mut bw = BufWriter::new(amkbd.stdin.unwrap());
                        bw.write_all(password.as_bytes()).expect("cannot send login to stdin of amkbd");
                    }
                    //std::thread::sleep(std::time::Duration::from_millis(2000));
                    tx2.send(Front2Back::Ready).await.expect("failed to send READY");
                }
                Some(Front2Back::Ready) => {
                    //println!("reported Ready");
                    let sts = sites2.clone();
                    let tx2 = tx1.clone();
                    cb_sink.send(Box::new(move |s| {
                        s.pop_layer();
                        s.add_layer(initial_dialog(sts, tx2));
                    })).expect("failed to update UI to Ready");
                }
                Some(Front2Back::ShowSecretData(secret)) => {
                    let tx2 = tx1.clone();
                    cb_sink.send(Box::new(move |s| {
                        s.pop_layer();
                        s.add_layer(show_secret_data_dialog(secret, tx2));
                    })).expect("failed to show secret data");
                }
                None => {}
            }
        }
    });
    siv.run();
    Ok(())
}
