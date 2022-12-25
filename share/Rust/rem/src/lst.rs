use cursive::{Cursive, CursiveRunnable};
use cursive::views::{EditView,Dialog,LinearLayout,TextView,Button,Panel,PaddedView};
use cursive::theme::{Theme,BorderStyle,Palette,BaseColor::*,Color::*,PaletteColor::*};
use postgres::Client;
use std::rc::Rc;
use std::cell::{RefMut,RefCell};

use listserv::{get_client,vibrate,mksiv,LThemes,Waiter,dismiss,wait,show_banner};

fn delete_item(title: String, x: String, connection: Rc<RefCell<Client>>) {
    let mut s = mksiv(LThemes::Alert);
    s.pop_layer();
    let newconn1 = Rc::clone(&connection);
    let newconn2 = Rc::clone(&connection);
    let t1 = title.clone();
    
    s.add_layer(Dialog::text(format!("Really delete {} ?",x))
        .title("Confirmation")
        .button("Yes!", move |win| {
            vibrate();
            win.quit();
            let wtr = show_banner("UPDATE-ing...");
            newconn1.borrow_mut().execute("update lists set items = array_remove(items,$1) where title = $2", &[&x,&t1]).expect("failed to remove");
            dismiss(wtr);
            show_list(String::from(&t1), Rc::clone(&newconn1));
        })
        .button("No!", move |win| {
            vibrate();
            win.quit();
            show_list(String::from(&title), Rc::clone(&newconn2));
        }));
    s.run();
}
fn delete_list(title: String, connection: Rc<RefCell<Client>>) {
    let mut s = mksiv(LThemes::Alert);
    s.pop_layer();
    let newconn1 = Rc::clone(&connection);
    let newconn2 = Rc::clone(&connection);
    let t1 = title.clone();
    
    s.add_layer(Dialog::text(format!("Really delete list {} ?",title))
        .title("Confirmation")
        .button("Yes!", move |win| {
            vibrate();
            win.quit();
            let wtr = show_banner("DELETE-ing...");
            newconn1.borrow_mut().execute("delete from lists where title = $1", &[&t1]).expect("failed to delete");
            dismiss(wtr);
            show_lists(Rc::clone(&newconn1));
        })
        .button("No!", move |win| {
            vibrate();
            win.quit();
            show_lists(Rc::clone(&newconn2));
        }));
    s.run();
}
fn show_list(title: String, connection: Rc<RefCell<Client>>) {
    let mut s = mksiv(LThemes::Normal);
    s.set_window_title(&title);
    s.pop_layer();
    let mut vbox = LinearLayout::vertical();
    let wtr = show_banner("SELECT-ing...");
    let lsts = connection.borrow_mut().query("select * from lists where title = $1 ", &[&title]).expect("failed to query lists");
    dismiss(wtr);
    vbox = vbox.child(TextView::new(title.clone()));
    let title1 = title.clone();
    let newconn = Rc::clone(&connection);
    vbox = vbox.child(EditView::new().on_submit(move |win, x| {
        vibrate();
        win.quit();
        let wtr = show_banner("UPDATE-ing...");
        newconn.borrow_mut().execute("update lists set items = array_append(items,$1) where title = $2", &[&x, &title1]).expect("failed to add an item");    
        dismiss(wtr);
        show_list(String::from(&title1), Rc::clone(&newconn));
    }));
    if let Some(lst) = lsts.iter().next() {
        let items: Vec<String> = lst.get(1);
        for item in items {
            let newconn1 = Rc::clone(&connection);
            let x = item.clone();
            let title2 = title.clone();
            vbox = vbox.child(Button::new(item.clone(), move |win| {
                vibrate();
                win.quit();
                delete_item(String::from(&title2), String::from(&x), Rc::clone(&newconn1));
            }));
        }
    }
    //vbox = vbox.child(TextView::new("-----------"));
    let newconn = Rc::clone(&connection);
    let newconn1 = Rc::clone(&connection);
    let title2 = title.clone();
    vbox = vbox.child(Panel::new(Button::new("delete this list", move |win| {
        vibrate();
        win.quit();
        delete_list(String::from(&title2), Rc::clone(&newconn));
    })));
    //vbox = vbox.child(TextView::new("==========="));
    vbox = vbox.child(Panel::new(Button::new("back to lists", move |win| {
        vibrate();
        win.quit();
        show_lists(Rc::clone(&newconn1));
    })));
    s.add_layer(Dialog::around(vbox));
    s.run();
}

fn show_lists(connection: Rc<RefCell<Client>>) {
    let mut s = mksiv(LThemes::Normal);
    s.set_window_title("LISTS");
    s.pop_layer();
    let mut vbox = LinearLayout::vertical();
    let newconn = Rc::clone(&connection);
    vbox = vbox.child(EditView::new().on_submit(move |win, t| {
        vibrate();
        win.quit();
        let wtr = show_banner("UPDATE-ing...");
        newconn.borrow_mut().execute("insert into lists (title, items) values ($1, '{}'::text[])", &[&t]).expect("failed to create new list");    
        dismiss(wtr);
        show_lists(Rc::clone(&newconn));
    }));
    
    let wtr = show_banner("SELECT-ing...");
    let rows = connection.borrow_mut().query("select * from lists", &[]).expect("failed to query lists");
    dismiss(wtr);
    for row in rows {
        let ttl: String = row.get(0);
        let newconn = Rc::clone(&connection);
        vbox = vbox.child(PaddedView::lrtb(0,0,1,0,Button::new(ttl.clone(), move |win| {
            vibrate();
            win.quit();
            show_list(ttl.clone(), Rc::clone(&newconn));
        })));
    };
    vbox = vbox.child(Panel::new(Button::new("EXIT", move |win| {
        vibrate();
        exit_program(win);
    })));
    s.add_layer(Dialog::around(vbox));
    s.run();
}

fn exit_program(s: &mut Cursive) {
    s.quit();

}

fn main() {

    let connection  = get_client().expect("failed to create tls postgres connection");
    let conn = Rc::new(RefCell::new(connection));

    show_lists(conn);

}

