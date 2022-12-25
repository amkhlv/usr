use postgres::Client;
use std::io;
use cursive::{CbSink,Cursive,CursiveRunnable};
use cursive::theme::{Theme,BorderStyle,Palette,BaseColor::*,Color::*,PaletteColor::*};
use cursive::views::{EditView,Dialog,LinearLayout,TextView,Button,Panel,PaddedView};
use std::rc::Rc;
use std::cell::RefCell;
use std::sync::mpsc::{Sender, Receiver};
use std::sync::mpsc;
use std::thread;

use listserv::{get_client,vibrate,mksiv,wait,show_banner,dismiss,LThemes,Waiter};

fn delete_todo(x: String, connection: Rc<RefCell<Client>>) {
    let mut w = mksiv(LThemes::Alert);
    w.pop_layer();
    let newconn1 = Rc::clone(&connection);
    let newconn2 = Rc::clone(&connection);
    w.add_layer(Dialog::text(format!("Really delete: {} ?",x))
        .title("Confirmation")
        .button("Yes!", move |win| {
            vibrate();
            win.quit();
            let deleting = show_banner("DELETE-ing...");
            newconn1.borrow_mut().execute("delete from todolist where todo = $1", &[&x]).expect("failed to delete");
            show_list(Some(deleting), Rc::clone(&newconn1));
        })
        .button("No!", move |win| {
            vibrate();
            win.quit();
            show_list(None, Rc::clone(&newconn2));
        }));
    w.run();
}
fn show_list(previous: Option<Waiter>, connection: Rc<RefCell<Client>>) {
    let mut vbox = LinearLayout::vertical();
    previous.map(dismiss);
    let selecting = show_banner("SELECT-ing...");
    let todos = connection.borrow_mut().query("select todo from todolist", &[]).expect("failed to query todos");
    let newconn = Rc::clone(&connection);
    vbox = vbox.child(EditView::new().on_submit(move |win, x| {
        vibrate();
        win.quit();
        let inserting = show_banner("INSERT-ing...");
        newconn.borrow_mut().execute("insert into todolist (todo) values ($1)", &[&x]).expect("failed to add an item");    
        show_list(Some(inserting), Rc::clone(&newconn));
    }));
    for row in todos {
        let todo: String = row.get(0);
        let newconn1 = Rc::clone(&connection);
        let x = todo.clone();
        vbox = vbox.child(Button::new(todo.clone(), move |win| {
            vibrate();
            win.quit();
            delete_todo(String::from(&x), Rc::clone(&newconn1));
        }));
    }
    vbox = vbox.child(Panel::new(Button::new("EXIT", move |win| {
        vibrate();
        win.quit();
    })));
    let mut w = mksiv(LThemes::Normal);
    w.set_window_title("TODOLIST");
    w.add_layer(Dialog::around(vbox));
    dismiss(selecting);
    w.run();
}
fn main() {
    let connection : Client = get_client().expect("failed to create tls postgres connection");
    let conn = Rc::new(RefCell::new(connection));
    show_list(None, conn);
}
