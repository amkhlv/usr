use clap::Parser;
use docx_rs::*;
use serde::{Deserialize, Serialize};
use serde_xml_rs::{from_reader, from_str, to_string};
use std::fs::File;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// input XML file
    #[arg(short, long)]
    input: String,

    /// Number of times to greet
    #[arg(short, long)]
    output: String,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct Root {
    #[serde(rename = "$value")]
    topitems: Vec<DocChild>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "kebab-case")]
enum DocChild {
    P(P),
    Table(Table),
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct Table {
    #[serde(rename = "$value")]
    rows: Vec<Tr>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct Tr {
    #[serde(rename = "$value")]
    cells: Vec<Td>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct Td {
    #[serde(rename = "$value")]
    content: Vec<DocChild>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "kebab-case")]
enum ParaChild {
    R(R),
    A(A),
}
#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct R {
    size: Option<usize>,
    color: Option<String>,
    b: Option<String>,
    i: Option<String>,
    u: Option<String>,
    #[serde(rename = "$value")]
    value: String,
}
#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct A {
    href: String,
    #[serde(rename = "$value")]
    value: String,
}

const ALIGN_CENTER: &str = "center";
const ALIGN_LEFT: &str = "left";
const ALIGN_RIGHT: &str = "right";
#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct P {
    size: Option<usize>,
    color: Option<String>,
    align: Option<String>,
    #[serde(rename = "$value")]
    children: Vec<ParaChild>,
}

fn mkrun(run: R, parent_prop: &RunProperty) -> Run {
    let mut r = Run::new();
    r = r.add_text(run.value);
    if let Some(size) = run.size {
        r = r.size(size)
    } else {
        r.run_property.sz = parent_prop.clone().sz
    }
    if let Some(color) = run.color {
        r = r.color(color)
    } else {
        r.run_property.color = parent_prop.clone().color
    }
    if run.b.is_some() {
        r = r.bold()
    }
    if run.i.is_some() {
        r = r.italic()
    }
    if let Some(line_type) = run.u {
        r = r.underline(line_type)
    }
    r
}
fn mkpara(para: P) -> Paragraph {
    let mut p = Paragraph::new();
    let mut run_prop = RunProperty::new();
    if let Some(size) = para.size {
        run_prop = run_prop.size(size);
    }
    if let Some(color) = para.color {
        run_prop = run_prop.color(color);
    }
    if let Some(align) = para.align {
        match align.as_str() {
            ALIGN_CENTER => p = p.align(AlignmentType::Center),
            ALIGN_LEFT => p = p.align(AlignmentType::Left),
            ALIGN_RIGHT => p = p.align(AlignmentType::Right),
            _ => {}
        }
    }
    p = p.run_property(run_prop.clone());
    for child in para.children {
        match child {
            ParaChild::R(run) => p = p.add_run(mkrun(run, &run_prop)),
            ParaChild::A(a) => {
                let h = Hyperlink::new(a.href, HyperlinkType::External).add_run(mkrun(
                    R {
                        value: a.value,
                        color: Some(String::from("0000FF")),
                        size: None,
                        b: None,
                        i: None,
                        u: Some(String::from("")),
                    },
                    &run_prop,
                ));
                p = p.add_hyperlink(h)
            }
        }
    }
    p
}
fn mktable(tbl: Table) -> docx_rs::Table {
    let mut rs: Vec<docx_rs::TableRow> = Vec::new();
    for row in tbl.rows {
        let mut cs: Vec<TableCell> = Vec::new();
        for cell in row.cells {
            let mut c = TableCell::new();
            for x in cell.content {
                match x {
                    DocChild::P(p) => c = c.add_paragraph(mkpara(p)),
                    DocChild::Table(t) => c = c.add_table(mktable(t)),
                }
            }
            cs.push(c)
        }
        rs.push(TableRow::new(cs));
    }
    docx_rs::Table::new(rs)
}
fn main() -> Result<(), DocxError> {
    let args = Args::parse();
    let input = File::open(args.input).expect("input file not found");

    let doc: Root = from_reader(input).unwrap();
    let path = std::path::Path::new(&args.output);
    let output = std::fs::File::create(path).unwrap();
    let mut docx = Docx::new();
    for topitem in doc.topitems {
        match topitem {
            DocChild::P(p) => docx = docx.add_paragraph(mkpara(p)),
            DocChild::Table(t) => docx = docx.add_table(mktable(t)),
        }
    }
    docx.build().pack(output).unwrap();
    Ok(())
}
