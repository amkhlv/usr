use postgres::Client;
use std::io;
use cursive::Cursive;

use cursive::theme::{Theme,BorderStyle,Palette,BaseColor::*,Color::*,PaletteColor::*};
use cursive::views::{EditView,Dialog,LinearLayout,TextView,Button,Panel,PaddedView};
use std::rc::Rc;
use std::cell::RefCell;

use listserv::get_client;

fn print_list(mut connection : Client) -> () {
        let result = connection.query("select todo from todolist", &[]).expect("failed to query todos");
        let mut i = 0;
        for row in  result {
            i = i + 1;
            let todo: &str = row.get(0);
            println!("{}. {}", i, todo);
        };
        connection.close().expect("could not close psql connection");
}
fn delete_todo(s: &mut Cursive, x: String, connection: Rc<RefCell<Client>>) {
    s.pop_layer();
    let newconn1 = Rc::clone(&connection);
    let newconn2 = Rc::clone(&connection);
    
    s.add_layer(Dialog::text(format!("Really delete: {} ?",x))
        .title("Confirmation")
        .button("Yes!", move |win| {
            newconn1.borrow_mut().execute("delete from todolist where todo = $1", &[&x]).expect("failed to delete");
            show_list(win, Rc::clone(&newconn1));
        })
        .button("No!", move |win| {
            show_list(win, Rc::clone(&newconn2));
        }));
}
fn show_list(s: &mut Cursive, connection: Rc<RefCell<Client>>) {
    s.set_window_title("TODOLIST");
    s.pop_layer();
    let mut vbox = LinearLayout::vertical();
    let todos = connection.borrow_mut().query("select todo from todolist", &[]).expect("failed to query todos");
    let newconn = Rc::clone(&connection);
    vbox = vbox.child(EditView::new().on_submit(move |win, x| {
        newconn.borrow_mut().execute("insert into todolist (todo) values ($1)", &[&x]).expect("failed to add an item");    
        show_list(win, Rc::clone(&newconn));
    }));
    for row in todos {
        let todo: String = row.get(0);
        let newconn1 = Rc::clone(&connection);
        let x = todo.clone();
        vbox = vbox.child(Button::new(todo.clone(), move |win| {
            delete_todo(win, String::from(&x), Rc::clone(&newconn1));
        }));
    }
    vbox = vbox.child(Panel::new(Button::new("EXIT", move |win| {
        win.quit();
    })));
    s.add_layer(Dialog::around(vbox));
}

fn main() {
    let connection : Client = get_client().expect("failed to create tls postgres connection");
    let conn = Rc::new(RefCell::new(connection));
    let mut siv = cursive::termion();
    siv.set_fps(10);
    let mut palette = Palette::default();
    palette.extend(vec![(Background,Light(Yellow))]);
    siv.set_theme(Theme {shadow: false, borders: BorderStyle::Outset, palette});
    show_list(&mut siv, conn);
    siv.run();
}
