use chrono::naive::NaiveDate;
use chrono::{Days, Months};
use clap::{IntoApp, Parser};
use clap_complete::{generate, shells::Bash};
use glob::glob;
use json::object;
use json::JsonValue;
use mail_parser::*;
use postgres_types::{FromSql, ToSql};
use regex::Regex;
use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::From;
use std::path::PathBuf;
use std::{fs, io};
use tokio_postgres::{Client, NoTls};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
#[clap(
    author,
    version,
    about = "
        email index and search; use --help for long help with SQL examples etc
        ",
    long_about = "
       The database name is \"emails\", the main table name is \"mail\":

            psql emails
            emails=> select * from mail where fwmatch(fw, 'amkhlv') and twmatch(tw, 'somebody') and mimematch (ctypes, 'pdf') ;

        The maildirs paths are stored in table \"maildirs\", and default search folder in table \"settings\".

        To show table schemas and function definitions:

            amail --show-schemas

        Notice that psql can read from stdin :

            echo \"select * from mail  where  subjmatch(s, 'reminder')  and  d between '2023-01-05' and '2023-01-07'\" | psql -t emails

        Or, use vipe from moreutils:

            vipe | psql -t emails

        (Then press Ctrl-D)
       "
)]
struct Args {
    /// index
    #[clap(short, long)]
    index: bool,

    /// when indexing, how many days
    #[clap(long, value_name = "DAYS", default_value_t = 180)]
    days: u16,

    /// dates interval, eg 2020-01-20,2020-02-27
    /// separator ∈ [':' ',' '.'], date could be 20200120 or 2020-01 or 202001
    /// "2020-01-01:" means since 2020-01-01 till now, ":2020-01-01" means from 1970-01-01 till
    /// 2020-01-01
    #[clap(short, long, value_name = "DATED")]
    dated: Option<String>,

    /// from matches
    #[clap(short, long, value_name = "FROM")]
    from: Option<String>,

    /// to matches
    #[clap(short, long, value_name = "TO")]
    to: Option<String>,

    /// subject matches
    #[clap(short, long, value_name = "SUBJECT")]
    subject: Option<String>,

    /// cc matches
    #[clap(long, value_name = "CC")]
    cc: Option<String>,

    /// bcc matches
    #[clap(long, value_name = "BCC")]
    bcc: Option<String>,

    /// MIME types matches
    #[clap(short, long, value_name = "MIME")]
    mime: Option<String>,

    /// Regexp in body matches (slow)
    #[clap(short, long, value_name = "TEXT")]
    body: Option<String>,

    /// print message text
    #[clap(long)]
    text: bool,

    /// print message HTML
    #[clap(long)]
    html: bool,

    /// save to default search folder
    #[clap(long)]
    save: bool,

    /// save to search folder
    #[clap(long, value_name = "SEARCH_FOLDER")]
    save_to: Option<String>,

    /// output JSON
    #[clap(short, long)]
    json: bool,

    /// output path
    #[clap(short, long)]
    path: bool,

    /// look at emails in this directory; parse them and dump in JSON format; incompatible with --index
    #[clap(long)]
    dir: Option<String>,

    /// everything after the word WHERE is read from STDIN
    #[clap(short)]
    w: bool,

    /// order by
    #[clap(short, long)]
    order_by: Option<String>,

    /// show schemas
    #[clap(long)]
    show_schemas: bool,

    /// clean the index (delete all)
    #[clap(long)]
    clean: bool,

    /// Generate bash completion
    #[clap(long)]
    completion: bool,
}

#[derive(Debug, ToSql, FromSql)]
#[postgres(name = "address")]
struct PGAddress {
    name: String,
    email: String,
}
impl std::fmt::Display for PGAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} <{}>", self.name, self.email)
    }
}
impl From<PGAddress> for JsonValue {
    fn from(a: PGAddress) -> JsonValue {
        object! {
            name: a.name,
            email: a.email,
        }
    }
}

#[derive(Debug, ToSql, FromSql)]
#[postgres(name = "grp")]
struct PGGroup {
    name: String,
    addresses: Vec<PGAddress>,
}
impl std::fmt::Display for PGGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Group ``{}''", self.name)
    }
}
impl From<PGGroup> for JsonValue {
    fn from(a: PGGroup) -> JsonValue {
        object! {
            name: a.name,
            addresses: a.addresses,
        }
    }
}
#[derive(Debug, ToSql, FromSql)]
#[postgres(name = "towhom")]
struct PGToWhom {
    addresses: Vec<PGAddress>,
    groups: Vec<PGGroup>,
}
impl std::fmt::Display for PGToWhom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[ {} ],  groups: [ {} ]",
            self.addresses
                .iter()
                .map(|a| { format!("{}", a) })
                .collect::<Vec<String>>()
                .join(", "),
            self.groups
                .iter()
                .map(|a| { format!("{}", a) })
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}
trait ToPGAddress {
    fn pgaddr(&self) -> PGAddress;
}
impl<'a> ToPGAddress for Addr<'a> {
    fn pgaddr(&self) -> PGAddress {
        PGAddress {
            name: self
                .name
                .clone()
                .map(|c| c.to_string())
                .unwrap_or("".to_owned()),
            email: self
                .address
                .clone()
                .map(|c| c.to_string())
                .unwrap_or("".to_owned()),
        }
    }
}
trait ToPGToWhom {
    fn pgtowhom(&self) -> PGToWhom;
}
impl<'a> ToPGToWhom for Addr<'a> {
    fn pgtowhom(&self) -> PGToWhom {
        PGToWhom {
            addresses: vec![self.pgaddr()],
            groups: vec![],
        }
    }
}
impl<'a> ToPGToWhom for Vec<Addr<'a>> {
    fn pgtowhom(&self) -> PGToWhom {
        PGToWhom {
            addresses: self.iter().map(|a| a.pgaddr()).collect(),
            groups: vec![],
        }
    }
}
impl<'a> ToPGToWhom for Group<'a> {
    fn pgtowhom(&self) -> PGToWhom {
        PGToWhom {
            addresses: vec![],
            groups: vec![PGGroup {
                name: self.name.clone().unwrap_or(Cow::from("")).to_string(),
                addresses: self.addresses.iter().map(|a| a.pgaddr()).collect(),
            }],
        }
    }
}
impl<'a> ToPGToWhom for Vec<Group<'a>> {
    fn pgtowhom(&self) -> PGToWhom {
        PGToWhom {
            addresses: vec![],
            groups: self
                .iter()
                .map(|g| PGGroup {
                    name: g.name.clone().unwrap_or(Cow::from("")).to_string(),
                    addresses: g.addresses.iter().map(|a| a.pgaddr()).collect(),
                })
                .collect(),
        }
    }
}

fn get_to(hv: &HeaderValue) -> PGToWhom {
    match hv {
        HeaderValue::Address(x) => x.pgtowhom(),
        HeaderValue::AddressList(xs) => xs.pgtowhom(),
        HeaderValue::Group(x) => x.pgtowhom(),
        HeaderValue::GroupList(xs) => xs.pgtowhom(),
        HeaderValue::Empty => PGToWhom {
            addresses: vec![],
            groups: vec![],
        },
        a => {
            eprintln!("{:?}", a);
            panic!("not single address");
        }
    }
}

async fn index_dir(
    mdkey: Option<&String>,
    dir: &PathBuf,
    client: &Client,
    days: u16,
    output_json: bool,
) -> Result<(), Box<dyn std::error::Error>> {
    //println!("doing {:?}", dir);
    for entry in fs::read_dir(dir).unwrap() {
        let entry = entry?;
        let path = entry.path();
        let metadata = fs::metadata(&path)?;
        let is_within = if let Ok(Ok(elapsed)) = metadata.created().map(|x| x.elapsed()) {
            elapsed.as_secs() < (days as u64) * 86400
        } else {
            false
        };
        if metadata.is_file() && is_within {
            let raw = fs::read(&path).unwrap();
            let msg = Message::parse(&raw).ok_or("unable to parse message")?;
            let f: PGAddress = match (&msg).from() {
                HeaderValue::Address(x) => x.pgaddr(),
                HeaderValue::AddressList(xs) => {
                    println!("\n{}", path.to_string_lossy());
                    println!("Multiple From: addresses (taking the first one) {:?}", xs);
                    xs[0].pgaddr()
                }
                other => {
                    eprintln!("{}", path.to_string_lossy());
                    eprintln!("other: {:?}", other);
                    panic!("TODO");
                }
            };
            let i = (&msg).message_id();
            let mut subtypes: Vec<String> = vec![];
            let cts = (&msg)
                .parts
                .iter()
                .map(|p| p.content_type().map(|tp| tp.subtype()));
            for tp in cts {
                if let Some(Some(x)) = tp {
                    subtypes.push(x.to_owned());
                }
            }
            if let Some(msgid) = i {
                let perm_path = &path
                    .to_str()
                    .unwrap()
                    .split(":")
                    .next()
                    .ok_or(format!("path: {:?} for msgid {}", &path, msgid))?;
                let existing = client
                    .query("select maildir from mail where i = $1", &[&msgid])
                    .await?;
                if existing.len() == 0 || output_json {
                    let t: PGToWhom = get_to((&msg).to());
                    let cc: PGToWhom = get_to((&msg).cc());
                    let bcc: PGToWhom = get_to((&msg).bcc());
                    let s = (&msg).subject();
                    if let Some(d) = (&msg)
                        .date()
                        .map(|x| format!("{}-{:02}-{:02}", x.year, x.month, x.day))
                    {
                        if output_json {
                            let json = object! {
                                msgid: i,
                                path: *perm_path,
                                from: f,
                                to_addresses: t.addresses,
                                to_groups: t.groups,
                                cc: cc.addresses,
                                subject: s,
                                date: d
                            };
                            println!("{}", json.dump());
                        } else {
                            print!(".");
                            client.execute(
                                "insert into mail (maildir,tw,cc,bcc,fw,s,i,d,ctypes,p) values ($1,$2,$3,$4,$5,$6,$7,TO_DATE($8,'YYYY-MM-DD'),$9,$10)", 
                                &[&mdkey, &t, &cc, &bcc, &f, &s, &i, &d, &subtypes, perm_path]
                            ).await?;
                        }
                    } else {
                        eprintln!("SKIPPING UNDATED: {}", perm_path);
                    }
                }
            }
        }
    }
    Ok(())
}

async fn index(
    maildir: HashMap<String, PathBuf>,
    client: Client,
    days: u16,
    dir: Option<String>,
) -> Result<(), Box<dyn std::error::Error>> {
    if let Some(folder) = dir {
        index_dir(None, &std::path::PathBuf::from(folder), &client, days, true).await?;
    } else {
        for mdkey in maildir.keys() {
            println!("indexing {}", mdkey);
            let md = maildir.get(mdkey).unwrap();
            index_dir(Some(mdkey), md, &client, days, false).await?;
        }
    }
    Ok(())
}
struct SearchTerms {
    to_whom: Option<Vec<String>>,
    from_whom: Option<Vec<String>>,
    time_interval: Option<(String, String)>,
    subject: Option<String>,
    mime_types: Option<Vec<String>>,
}
fn parse_date(x: &str, latest: bool) -> String {
    let re_yyyymmdd = Regex::new(r"^(?P<y>\d{4})(?P<m>\d{2})(?P<d>\d{2})$").unwrap();
    let re_yyyy_mm_dd = Regex::new(r"^(?P<y>\d{4})-(?P<m>\d{2})-(?P<d>\d{2})$").unwrap();
    let re_yyyymm = Regex::new(r"^(?P<y>\d{4})(?P<m>\d{2})$").unwrap();
    let re_yyyy_mm = Regex::new(r"^(?P<y>\d{4})-(?P<m>\d{2})$").unwrap();
    let ymd_caps = re_yyyy_mm_dd.captures(x).or(re_yyyymmdd.captures(x));
    let ym_caps = re_yyyy_mm.captures(x).or(re_yyyymm.captures(x));
    if ymd_caps.is_none() && ym_caps.is_none() {
        if latest {
            let now = chrono::offset::Local::now() + Days::new(1);
            return format!("{}", now.format("%Y-%m-%d"));
        } else {
            return "1970-01-01".to_owned();
        }
    }
    if let Some(caps) = ymd_caps {
        return format!(
            "{}-{}-{}",
            caps.name("y").unwrap().as_str(),
            caps.name("m").unwrap().as_str(),
            caps.name("d").unwrap().as_str()
        );
    }
    if let Some(caps) = ym_caps {
        let mut ndt = NaiveDate::from_ymd_opt(
            caps.name("y").unwrap().as_str().parse::<i32>().unwrap(),
            caps.name("m").unwrap().as_str().parse::<u32>().unwrap(),
            1,
        )
        .unwrap();
        if latest {
            ndt = ndt + Months::new(1) - Days::new(1);
        }
        return format!("{}", ndt);
    }
    eprintln!("ERROR: Unable to parse date {}", x);
    panic!();
}
fn search_terms(terms: SearchTerms) -> (String, Vec<String>) {
    let mut qs = String::from("");
    let mut ps: Vec<String> = vec![];
    let mut j = 0;
    if let Some(tws) = terms.to_whom {
        let mut x = String::from("");
        for tw in &tws {
            j = j + 1;
            let y = format!(" twmatch(tw,${}) ", j);
            ps.push(tw.clone());
            if x.len() > 0 {
                x = format!(" {} or {} ", x, y);
            } else {
                x = y;
            }
        }
        if tws.len() > 1 {
            qs = format!(" ( {} ) ", x);
        } else {
            qs = x;
        };
    };
    if let Some(fws) = terms.from_whom {
        let mut x = String::from("");
        for tw in &fws {
            j = j + 1;
            let y = format!(" fwmatch(fw,${}) ", j);
            ps.push(tw.clone());
            if x.len() > 0 {
                x = format!(" {} or {} ", x, y);
            } else {
                x = y;
            }
        }
        let prevqs = if qs.len() > 0 {
            format!("{} and ", qs)
        } else {
            "".to_owned()
        };
        if fws.len() > 1 {
            qs = format!("{} ( {} ) ", prevqs, x);
        } else {
            qs = format!("{} {} ", prevqs, x);
        };
    };
    if let Some(mimes) = terms.mime_types {
        let mut x = String::from("");
        for mm in &mimes {
            j = j + 1;
            let y = format!(" mimematch(ctypes,${}) ", j);
            ps.push(mm.clone());
            if x.len() > 0 {
                x = format!(" {} or {} ", x, y);
            } else {
                x = y;
            }
        }
        let prevqs = if qs.len() > 0 {
            format!("{} and ", qs)
        } else {
            "".to_owned()
        };
        if mimes.len() > 1 {
            qs = format!("{} ( {} ) ", prevqs, x);
        } else {
            qs = format!("{} {} ", prevqs, x);
        };
    };
    if let Some((start, end)) = terms.time_interval {
        j = j + 2;
        let y = format!(
            " d BETWEEN TO_DATE(${},'YYYY-MM-DD') AND TO_DATE(${},'YYYY-MM-DD') ",
            j - 1,
            j
        );
        ps.push(parse_date(&start, false));
        ps.push(parse_date(&end, true));
        let prevqs = if qs.len() > 0 {
            format!("{} and ", qs)
        } else {
            "".to_owned()
        };
        qs = format!("{} {}", prevqs, y);
    };
    if let Some(subj) = terms.subject {
        j = j + 1;
        let y = format!(" subjmatch(s, ${}) ", j);
        //let y = format!(" to_tsvector(s) @@ to_tsquery(${}) ", j);
        ps.push(subj.clone());
        let prevqs = if qs.len() > 0 {
            format!("{} and ", qs)
        } else {
            "".to_owned()
        };
        qs = format!("{} {}", prevqs, y);
    };
    (qs, ps)
}
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let clops = Args::parse();
    if clops.completion {
        generate(Bash, &mut Args::into_app(), "amail", &mut io::stdout());
        return Ok(());
    }

    if clops.show_schemas {
        let tpschema = std::process::Command::new("psql")
            .args([
                "emails",
                "-c",
                "\\d address",
                "-c",
                "\\d grp",
                "-c",
                "\\d towhom",
            ])
            .output()
            .expect("could not get type definitions");
        let stschema = std::process::Command::new("psql")
            .args(["emails", "-c", "\\d settings"])
            .output()
            .expect("could not get schema for table: settings");
        let mdschema = std::process::Command::new("psql")
            .args(["emails", "-c", "\\d maildirs"])
            .output()
            .expect("could not get schema for table: maildirs");
        let dbschema = std::process::Command::new("psql")
            .args(["emails", "-c", "\\d mail"])
            .output()
            .expect("could not get schema for table: mail");
        let fnschema = std::process::Command::new("psql")
            .args(["emails", "-c", "\\df"])
            .output()
            .expect("could not get function definitions");
        println!("\n{}", std::str::from_utf8(&tpschema.stdout).unwrap());
        println!("{}", std::str::from_utf8(&stschema.stdout).unwrap());
        println!("{}", std::str::from_utf8(&mdschema.stdout).unwrap());
        println!("{}", std::str::from_utf8(&dbschema.stdout).unwrap());
        println!("{}", std::str::from_utf8(&fnschema.stdout).unwrap());
        return Ok(());
    }
    let clopsto = clops.to.clone();
    let clopsfrom = clops.from.clone();
    let clopsdated = clops.dated.clone();
    let clopssubject = clops.subject.clone();
    let clopsmime = clops.mime.clone();
    let (quer, pmtrs) = if clops.w {
        let mut q = String::new();
        for line in io::stdin().lines() {
            q = format!("{}{}", q, line.unwrap_or("".to_owned()));
        }
        (q, vec![])
    } else {
        let (q, p) = search_terms(SearchTerms {
            to_whom: clopsto.map(|x| x.split(",").map(|y| y.to_owned()).collect()),
            from_whom: clopsfrom.map(|x| x.split(",").map(|y| y.to_owned()).collect()),
            time_interval: clopsdated.map(|x| {
                let mut d = x.split(&[',', '.', ':'][..]).into_iter();
                let start = (&mut d).next().unwrap();
                let end = (&mut d).next().unwrap();
                (start.to_owned(), end.to_owned())
            }),
            subject: clopssubject,
            mime_types: clopsmime.map(|x| x.split(",").map(|y| y.to_owned()).collect()),
        });
        if let Some(o) = clops.order_by {
            (
                format!(
                    "{} {} order by {}",
                    q,
                    if clops.json {
                        "group by maildir,i,d,tw,cc,bcc,fw,s,p"
                    } else {
                        ""
                    },
                    o
                ),
                p,
            )
        } else {
            (q, p)
        }
    };
    if !(clops.json || clops.path) {
        println!("{:?}\n {:?}", quer, pmtrs);
    }

    let (client, connection) =
        tokio_postgres::connect("host=/var/run/postgresql user=andrei dbname=emails", NoTls)
            .await?;
    tokio::spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("connection error: {}", e);
        }
    });

    if clops.clean {
        println!("Deleting the index");
        client.execute("delete from mail", &[]).await?;
    }
    let mut maildir: HashMap<String, PathBuf> = HashMap::new();
    for row in client.query("select * from maildirs", &[]).await? {
        maildir.insert(
            row.get::<usize, String>(0),
            PathBuf::from(row.get::<usize, String>(1)).join("cur"),
        );
    }
    let search_folder = PathBuf::from(
        client
            .query(
                "select value from settings where key='default_search_folder'",
                &[],
            )
            .await?
            .iter()
            .next()
            .expect("could not get default search folder")
            .get::<usize, String>(0),
    )
    .join("new");
    let search_requested: bool = clops.w
        || vec![
            &clops.to,
            &clops.from,
            &clops.subject,
            &clops.dated,
            &clops.mime,
        ]
        .iter()
        .filter(|x| x.is_some())
        .next()
        .is_some();
    if search_requested {
        let p: Vec<&(dyn ToSql + Sync)> = pmtrs.iter().map(|x| x as &(dyn ToSql + Sync)).collect();
        if clops.json {
            let q: Vec<tokio_postgres::Row> = client
                .query(
                    &format!(
                        "select CAST(json_agg(mail) AS TEXT) from mail where {}",
                        quer
                    ),
                    &p[..],
                )
                .await?;
            for r in q {
                let json = r.get::<usize, String>(0);
                println!("{}", json);
            }
        }
        let q: Vec<tokio_postgres::Row> = client
            .query(
                &format!(
                    "select maildir, i, CAST(d AS TEXT), tw, cc, bcc, fw, s, p from mail where {}",
                    quer
                ),
                &p[..],
            )
            .await?;
        'rowiter: for r in q {
            let md = r.get::<usize, String>(0);
            let msgid = r.get::<usize, String>(1);
            let file = r.get::<usize, String>(8);
            let mut no_body_match: bool = false;
            let bdy = clops.body.clone();
            let file1 = file.clone();
            let path0 = glob(&format!("{}*", &file)).map(|mut x| {
                x.next().map(|r| {
                    let pth = r.expect(&format!("Glob error on {}, msgid {}", file1, msgid));
                    if let Some(txt) = bdy {
                        no_body_match = true;
                        let rawmsg = fs::read(&pth).unwrap();
                        let msg = Message::parse(&rawmsg)
                            .ok_or("unable to parse message")
                            .unwrap();
                        let re = Regex::new(&txt).expect("could not parse Regex");
                        for bi in msg.text_body {
                            msg.parts[bi].text_contents().map(|t| {
                                if re.is_match(t) {
                                    no_body_match = false;
                                }
                            });
                        }
                        for bi in msg.html_body {
                            msg.parts[bi].text_contents().map(|t| {
                                if re.is_match(t) {
                                    no_body_match = false;
                                }
                            });
                        }
                    }
                    pth
                })
            });
            if no_body_match {
                continue 'rowiter;
            }
            if !(clops.json || clops.path) {
                println!("________________ {}", md);
                println!("Date: {} rfc822msgid:{}", r.get::<usize, String>(2), msgid);
                print!("To: {};  ", r.get::<usize, PGToWhom>(3));
                print!("CC: {};  ", r.get::<usize, PGToWhom>(4));
                println!("BCC: {}", r.get::<usize, PGToWhom>(5));
                println!("From: {}", r.get::<usize, PGAddress>(6));
                println!(
                    "Subj: {}",
                    r.try_get::<usize, String>(7).unwrap_or("".to_owned())
                );
            }
            if let Ok(Some(path)) = path0 {
                // if !(clops.json || clops.path) {
                //     println!("Path: {}",path.to_string_lossy());
                // }
                if clops.text {
                    let rawmsg = fs::read(&path).unwrap();
                    let msg = Message::parse(&rawmsg).ok_or("unable to parse message")?;
                    for bi in msg.text_body {
                        let body = &msg.parts[bi];
                        body.text_contents().map(|txt| {
                            println!("{}", txt);
                        });
                    }
                }
                if clops.html {
                    let rawmsg = fs::read(&path).unwrap();
                    let msg = Message::parse(&rawmsg).ok_or("unable to parse message")?;
                    let mut htmls = msg.html_body;
                    if htmls.is_empty() {
                        htmls = msg.text_body;
                    }
                    for bi in htmls {
                        let body = &msg.parts[bi];
                        body.text_contents().map(|txt| {
                            println!("{}", txt);
                        });
                    }
                }
                if clops.path {
                    println!("{}", path.to_string_lossy());
                }
                if clops.save {
                    let mut save_path = PathBuf::new();
                    save_path.push(&search_folder);
                    if let Some(f) = path.file_name() {
                        save_path.push(f);
                        if !(clops.json || clops.path) {
                            println!("copying {} to {:?}", file, save_path);
                        }
                        std::fs::copy(&path, save_path)?;
                    } else {
                        eprintln!("Error: {} is not a file path", &file);
                    }
                };
                if let Some(folder) = clops.save_to.clone() {
                    let mut save_path = PathBuf::new();
                    save_path.push(&folder);
                    save_path.push("new");
                    if let Some(f) = path.file_name() {
                        save_path.push(f);
                        if !(clops.json || clops.path) {
                            println!("copying {} to {:?}", file, save_path);
                        }
                        std::fs::copy(path, save_path)?;
                    } else {
                        eprintln!("Error: {} is not a file path", &file);
                    }
                }
            } else {
                eprintln!("=== ERROR === globbing {} for msgid {}", file, msgid);
            }
        }
    }
    if clops.index || clops.dir.is_some() {
        index(maildir, client, clops.days, clops.dir).await?;
    }

    Ok(())
}
