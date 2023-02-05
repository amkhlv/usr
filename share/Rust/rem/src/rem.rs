use cursive::views::{EditView,Dialog,LinearLayout,TextView,Button,Panel,PaddedView};
use cursive::CbSink;
use tokio_postgres::{Error,Row};
use tokio::sync::mpsc;
use futures::executor::block_on;
use std::time::Duration;
use tokio::time::{Timeout,timeout,error::Elapsed};
use std::future::Future;

use listserv::{wait,get_tls,get_db_conf};


#[derive(Debug)]
enum DBComm {
    ShowToDos,
    DeleteToDo(String),
    ConfirmDeleteToDo(String),
    InsertToDo(String),
    ShowLists,
    ShowList(String),
    NewList(String),
    DeleteList(String),
    ConfirmDeleteList(String),
    InsertItemIntoList(String,String),
    DeleteItemFromList(String,String),
    ConfirmDeleteItemFromList(String,String)
}

trait TimeoutPanic<T> {
    fn check_timeout(self, s: &CbSink) -> T ;
}

impl<T> TimeoutPanic<T> for Result<T,Elapsed> {
    fn check_timeout(self, s: &CbSink) -> T {
        match self {
            Err(_) => {
                s.send(Box::new(move |s| { s.quit(); })).expect("unable to quit UI");        
                panic!("TIMEOUT")
            },
            Ok(x) => x
        }
    }
}
fn patiently<T: Future>(f: T) -> Timeout<T> { timeout(Duration::from_secs(30), f) }

#[tokio::main]
async fn main() -> Result<(), Error> {
    let tls = get_tls().unwrap();
    
    let psql = get_db_conf().expect("could not get config");
    let confstring = format!("host={} port={} user={}", &psql.host, &psql.port, &psql.user);
    
    let (tx_com, mut rx_com) = mpsc::channel::<DBComm>(1);
    let (tx_rows, mut rx_rows) = mpsc::channel::<Vec<Row>>(1);
    let tx1 = tx_com.clone();
    //let rtx = Arc::new(RefCell::new(tx1));

    let mut siv = cursive::termion();
    let cb_sink = siv.cb_sink().clone();


    tokio::spawn(async move {
        let (client, cnctn) = tokio_postgres::connect(&confstring, tls).await.unwrap();
        tokio::spawn(async move {
            if let Err(e) = cnctn.await {
                eprintln!("connection error: {}", e);
            }
        });
        tx_rows.send(vec![]).await.expect("could not send READY");
        loop {
            match rx_com.recv().await {
                Some(DBComm::ShowToDos) => {
                    wait(&cb_sink, "SELECT-ing");
                    let todos = patiently(client.query("select todo from todolist", &[]))
                        .await.check_timeout(&cb_sink).expect("could not SELECT");
                    let tx2 = tx1.clone();
                    cb_sink.send(Box::new(move |s| {
                        let mut vbox = LinearLayout::vertical();
                        let txedit = tx2.clone();
                        vbox = vbox.child(
                            EditView::new()
                            .on_submit(move |_win, x| {
                                block_on(txedit.send(DBComm::InsertToDo(x.to_owned())))
                                .expect("could not send INSERT");
                            })
                        );
                        for row in todos {
                            let todo: String = row.get(0);
                            let tx3 = tx2.clone();
                            vbox = vbox.child(Button::new(todo.clone(), move |_win| {
                                block_on(tx3.send(DBComm::ConfirmDeleteToDo(todo.clone())))
                                .expect("could not send DELETE");
                            }));
                        }
                        vbox = vbox.child(Panel::new(Button::new("EXIT", move |win| {
                            win.quit();
                        })));
                        s.pop_layer();
                        s.add_layer(Dialog::around(vbox));
                    })).expect("unable to build UI via sink");
                }
                Some(DBComm::InsertToDo(x)) => {
                    wait(&cb_sink, "INSERT-ing");
                    let tx2 = tx1.clone();
                    patiently(client.execute("insert into todolist (todo) values ($1)", &[&x]))
                        .await.check_timeout(&cb_sink).expect("could not INSERT");
                    tx2.send(DBComm::ShowToDos).await.expect("could not send SELECT when inserting");
                }
                Some(DBComm::NewList(name)) => {
                    wait(&cb_sink, "INSERT-ing");
                    let tx2 = tx1.clone();
                    patiently(client.execute("insert into lists (title, items) values ($1, '{}'::text[])", &[&name]))
                        .await.check_timeout(&cb_sink).expect("failed to create new list");    
                    tx2.send(DBComm::ShowList(name)).await.expect("could not send SELECT when inserting");
                }
                Some(DBComm::InsertItemIntoList(itm,lst)) => {
                    wait(&cb_sink, "INSERT-ing");
                    patiently(client.execute(
                        "update lists set items = array_append(items,$1) where title = $2", 
                        &[&itm, &lst]
                    )).await.check_timeout(&cb_sink).expect("failed to add an item");    
                    let tx2 = tx1.clone();
                    tx2.send(DBComm::ShowList(lst)).await.expect("could not send SELECT when inserting");
                }
                Some(DBComm::DeleteItemFromList(itm,lst)) => {
                    wait(&cb_sink, "DELETE-ing");
                    patiently(client.execute(
                        "update lists set items = array_remove(items,$1) where title = $2", 
                        &[&itm,&lst]
                    )).await.check_timeout(&cb_sink).expect("failed to delete item from list");
                    let tx2 = tx1.clone();
                    tx2.send(DBComm::ShowList(lst)).await.expect("could not send ShowList when deleting");
                }
                Some(DBComm::ConfirmDeleteToDo(x)) => {
                    let txyes = tx1.clone();
                    let txno = tx1.clone();
                    cb_sink.send(Box::new(move |s| {
                        let x1 = x.clone();
                        s.pop_layer();
                        s.add_layer(Dialog::text(format!("Really delete {} ?",x))
                        .title("Confirmation")
                        .button("Yes!", move |_win| {
                            block_on(txyes.send(DBComm::DeleteToDo(x1.clone())))
                            .expect("could not send DeleteToDo");
                            })
                        .button("No!", move |_win| {
                            block_on(txno.send(DBComm::ShowToDos))
                            .expect("could not send ShowToDos");
                            }))})).expect("failed to send via sink");
                }
                Some(DBComm::DeleteList(name)) => {
                    wait(&cb_sink, "DELETE-ing");
                    patiently(client.execute(
                        "delete from lists where title = $1", 
                        &[&name]
                    )).await.check_timeout(&cb_sink).expect("failed to delete");
                    tx1.send(DBComm::ShowLists).await.expect("error sending ShowLists");
                }
                Some(DBComm::ConfirmDeleteList(name)) => {
                    let txyes = tx1.clone();
                    let txno = tx1.clone();
                    cb_sink.send(Box::new(move |s| {
                        let x1 = name.clone();
                        s.pop_layer();
                        s.add_layer(Dialog::text(format!("Delete WHOLE LIST {} ?",x1))
                        .title("Confirmation")
                        .button("Yes!", move |_win| {
                            block_on(txyes.send(DBComm::DeleteList(x1.clone())))
                            .expect("could not send DeleteToDo");
                            })
                        .button("No!", move |_win| {
                            block_on(txno.send(DBComm::ShowToDos))
                            .expect("could not send ShowToDos");
                            }))})).expect("failed to send via sink");
                }
                Some(DBComm::ConfirmDeleteItemFromList(itm,lst)) => {
                    let txyes = tx1.clone();
                    let txno = tx1.clone();
                    cb_sink.send(Box::new(move |s| {
                        let x1 = itm.clone();
                        let y1 = lst.clone();
                        let y2 = lst.clone();
                        s.pop_layer();
                        s.add_layer(Dialog::text(format!("Delete {} from {} ?",itm,lst))
                        .title("Confirmation")
                        .button("Yes!", move |_win| {
                            block_on(txyes.send(DBComm::DeleteItemFromList(x1.clone(),y1.clone())))
                            .expect("could not send DeleteItemFromList");
                            })
                        .button("No!", move |_win| {
                            block_on(txno.send(DBComm::ShowList(y2.clone())))
                            .expect("could not send ShowToDos");
                            }))})).expect("failed to send via sink");
                }
                Some(DBComm::DeleteToDo(x)) => {
                    wait(&cb_sink, "DELETE-ing");
                    let tx2 = tx1.clone();
                    patiently(client.execute("delete from todolist where todo = $1", &[&x]))
                        .await.check_timeout(&cb_sink).expect("could not DELETE");
                    tx2.send(DBComm::ShowToDos).await
                    .expect("error sending ShowToDos via sink");
                }
                Some(DBComm::ShowLists) => {
                    wait(&cb_sink, "SELECT-ing");
                    let rows = patiently(client.query("select * from lists", &[]))
                        .await.check_timeout(&cb_sink).expect("could not SELECT");
                    let tx2 = tx1.clone();
                    let txnl = tx1.clone();
                    cb_sink.send(Box::new(move |s| {
                        let mut vbox = LinearLayout::vertical();
                        vbox = vbox.child(
                            EditView::new()
                            .on_submit(move |_win, t| {
                                block_on(txnl.send(DBComm::NewList(t.to_owned())))
                                .expect("error sending NewList")
                            })
                        );
                        for row in rows {
                            let txsl = tx2.clone();
                            let ttl: String = row.get(0);
                            vbox = vbox.child(
                                PaddedView::lrtb(0,0,1,0,Button::new(
                                    ttl.clone(), 
                                    move |_win| {
                                        block_on(txsl.send(DBComm::ShowList(ttl.clone())))
                                        .expect("error sending ShowList")
                                    })));
                        };
                        vbox = vbox.child(Panel::new(
                            Button::new("EXIT", move |win| {
                                win.quit();
                            })
                        ));
                        s.pop_layer();
                        s.add_layer(Dialog::around(vbox));
                    }))
                    .expect("error sending ShowLists via sink")
                }
                Some(DBComm::ShowList(listname)) => {
                    wait(&cb_sink, "SELECT-ing");
                    let lsts = patiently(client.query("select * from lists where title = $1", &[&listname]))
                        .await.check_timeout(&cb_sink).expect("could not SELECT");
                    let tx2 = tx1.clone();
                    let tx3 = tx1.clone();
                    cb_sink.send(Box::new(move |s| {
                        let ln1 = listname.clone();
                        s.set_window_title(&listname);
                        let mut vbox = LinearLayout::vertical();
                        vbox = vbox.child(TextView::new(listname.clone()));
                        vbox = vbox.child(
                            EditView::new()
                            .on_submit(move |_win, itm| {
                                block_on(tx2.send(DBComm::InsertItemIntoList(itm.to_owned(), ln1.clone())))
                                .expect("error sending InsertItemIntoList");
                            })
                        );
                        if let Some(lst) = lsts.iter().next() {
                            let items: Vec<String> = lst.get(1);
                            for item in items {
                                let txdel = tx3.clone();
                                let x = item.clone();
                                let title2 = listname.clone();
                                vbox = vbox.child(Button::new(item.clone(), move |_win| {
                                    block_on(txdel.send(DBComm::ConfirmDeleteItemFromList(x.clone(),title2.clone())))
                                    .expect("error sending DeleteItemFromList");
                                }));
                            }
                        };
                        let txdelist = tx3.clone();
                        let which = listname.clone();
                        vbox = vbox.child(Panel::new(
                            Button::new("DELETE LIST", move |_win| {
                                block_on(txdelist.send(DBComm::ConfirmDeleteList(which.clone())))
                                .expect("error sending ConfirmDeleteList")
                            })
                        ));
                        let txback = tx3.clone();
                        vbox = vbox.child(Panel::new(
                            Button::new("BACK to lists", move |_win| {
                                block_on(txback.send(DBComm::ShowLists))
                                .expect("error sending ConfirmDeleteList")
                            })
                        ));
                        s.pop_layer();
                        s.add_layer(Dialog::around(vbox));
                    }))
                    .expect("error configuring GUI for ShowList via sink");
                }
                None => {}
            }
        }});

    siv.set_fps(30);
    let tx1 = tx_com.clone();
    let tx2 = tx_com.clone();
    println!("connecting ...");
    rx_rows.recv().await;
    siv.add_layer(
        Dialog::text(format!("welcome"))
            .title("Confirmation")
            .button("ToDo", move |_win| {
                block_on(tx1.send(DBComm::ShowToDos))
                .expect("could not send DeleteToDo");
                })
            .button("Lists", move |_win| {
                block_on(tx2.send(DBComm::ShowLists))
                .expect("could not send ShowToDos");
                }));      
    siv.run();
    println!("exiting");
    Ok(())
}
