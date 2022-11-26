use cursive::Cursive;
use cursive::views::{EditView,Dialog,LinearLayout,TextView,Button,Panel,PaddedView};
use cursive::theme::{Theme,BorderStyle,Palette,BaseColor::*,Color::*,PaletteColor::*};
use postgres::Client;
use std::rc::Rc;
use std::cell::RefCell;

use listserv::get_client;

fn delete_item(s: &mut Cursive, title: String, x: String, connection: Rc<RefCell<Client>>) {
    s.pop_layer();
    set_theme(LThemes::Normal, s);
    let newconn1 = Rc::clone(&connection);
    let newconn2 = Rc::clone(&connection);
    let t1 = title.clone();
    
    s.add_layer(Dialog::text(format!("Really delete {} ?",x))
        .title("Confirmation")
        .button("Yes!", move |win| {
            newconn1.borrow_mut().execute("update lists set items = array_remove(items,$1) where title = $2", &[&x,&t1]).expect("failed to remove");
            show_list(win, String::from(&t1), Rc::clone(&newconn1));
        })
        .button("No!", move |win| {
            show_list(win, String::from(&title), Rc::clone(&newconn2));
        }));
}
fn delete_list(s: &mut Cursive, title: String, connection: Rc<RefCell<Client>>) {
    s.pop_layer();
    set_theme(LThemes::Alert, s);
    let newconn1 = Rc::clone(&connection);
    let newconn2 = Rc::clone(&connection);
    let t1 = title.clone();
    
    s.add_layer(Dialog::text(format!("Really delete list {} ?",title))
        .title("Confirmation")
        .button("Yes!", move |win| {
            newconn1.borrow_mut().execute("delete from lists where title = $1", &[&t1]).expect("failed to delete");
            show_lists(win, Rc::clone(&newconn1));
        })
        .button("No!", move |win| {
            show_lists(win, Rc::clone(&newconn2));
        }));
}
fn show_list(s: &mut Cursive, title: String, connection: Rc<RefCell<Client>>) {
    s.set_window_title(&title);
    s.pop_layer();
    set_theme(LThemes::Normal, s);
    let mut vbox = LinearLayout::vertical();
    let lsts = connection.borrow_mut().query("select * from lists where title = $1 ", &[&title]).expect("failed to query lists");
    vbox = vbox.child(TextView::new(title.clone()));
    let title1 = title.clone();
    let newconn = Rc::clone(&connection);
    vbox = vbox.child(EditView::new().on_submit(move |win, x| {
        newconn.borrow_mut().execute("update lists set items = array_append(items,$1) where title = $2", &[&x, &title1]).expect("failed to add an item");    
        show_list(win, String::from(&title1), Rc::clone(&newconn));
    }));
    if let Some(lst) = lsts.iter().next() {
        let items: Vec<String> = lst.get(1);
        for item in items {
            let newconn1 = Rc::clone(&connection);
            let x = item.clone();
            let title2 = title.clone();
            vbox = vbox.child(Button::new(item.clone(), move |win| {
                delete_item(win, String::from(&title2), String::from(&x), Rc::clone(&newconn1));
            }));
        }
    }
    //vbox = vbox.child(TextView::new("-----------"));
    let newconn = Rc::clone(&connection);
    let newconn1 = Rc::clone(&connection);
    let title2 = title.clone();
    vbox = vbox.child(Panel::new(Button::new("delete this list", move |win| {
        delete_list(win, String::from(&title2), Rc::clone(&newconn));
    })));
    //vbox = vbox.child(TextView::new("==========="));
    vbox = vbox.child(Panel::new(Button::new("back to lists", move |win| {
        show_lists(win, Rc::clone(&newconn1));
    })));
    s.add_layer(Dialog::around(vbox));
}

fn show_lists(s: &mut Cursive, connection: Rc<RefCell<Client>>) {
    s.set_window_title("LISTS");
    set_theme(LThemes::Normal, s);
    s.pop_layer();
    let mut vbox = LinearLayout::vertical();
    let newconn = Rc::clone(&connection);
    vbox = vbox.child(EditView::new().on_submit(move |win, t| {
        newconn.borrow_mut().execute("insert into lists (title, items) values ($1, '{}'::text[])", &[&t]).expect("failed to create new list");    
        show_lists(win, Rc::clone(&newconn));
    }));
    
    let rows = connection.borrow_mut().query("select * from lists", &[]).expect("failed to query lists");
    for row in rows {
        let ttl: String = row.get(0);
        let newconn = Rc::clone(&connection);
        vbox = vbox.child(PaddedView::lrtb(0,0,1,0,Button::new(ttl.clone(), move |win| {
            win.update_theme(|theme| theme.palette.extend(vec![(Background,Dark(Yellow))]));
            show_list(win, ttl.clone(), Rc::clone(&newconn));
        })));
    };
    vbox = vbox.child(Panel::new(Button::new("EXIT", move |win| {
        exit_program(win);
    })));
    s.add_layer(Dialog::around(vbox));
}

fn exit_program(s: &mut Cursive) {
    s.quit();

}

enum LThemes {
    Alert,
    Normal,
    Wait
}
fn set_theme(x: LThemes, win: &mut  Cursive) {
    let mut palette = Palette::default();
    match x {
        LThemes::Alert => { palette.extend(vec![(Background,Light(Red))]); }
        LThemes::Wait  => { palette.extend(vec![(Background,Dark(Yellow))]); }
        LThemes::Normal => ()
    }
    win.set_theme(Theme {shadow: false, borders: BorderStyle::Outset, palette});
}

fn main() {

    let mut siv = cursive::termion();
    siv.set_fps(10);

    let connection  = get_client().expect("failed to create tls postgres connection");
    let conn = Rc::new(RefCell::new(connection));

    set_theme(LThemes::Normal, &mut siv);

    show_lists(&mut siv, conn);


    siv.run();
}

