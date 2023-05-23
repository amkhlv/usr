use clap::{Parser,IntoApp};
use clap_complete::{generate, shells::Bash};
use std::{io,fs};
use std::io::Read;
use docx_rs::*;
use std::collections::HashMap;
use serde::{Serialize, Deserialize};
use serde_dhall::StaticType;
use dirs::home_dir;


#[derive(Parser, Debug)]
#[clap(author, 
       version, 
       about = "
        docx generator
        \n
        reads single paragraph from stdin
       ",
       long_about = None)]
struct Args {

    /// Generate bash completion
    #[clap(long)]
    completion: bool,    

    /// Name of output file without .docx extension
    #[clap(short)]
    output: String,

    /// Title
    #[clap(long)]
    title: Option<String>,

    /// Salutation
    #[clap(long)]
    salutation: Option<String>,

    /// To 
    #[clap(long)]
    to: Option<String>,

    /// From 
    #[clap(long)]
    from: Option<String>,

    /// Signature 
    #[clap(long)]
    signature: Option<String>,

    #[clap(long, default_value_t=20)]
    size: usize,

}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, StaticType)]
struct DhallHyperLink {
    href: String,
    text: String,
}
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, StaticType)]
struct DhallImage {
    path: String,
    scale: f64,
}
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, StaticType)]
enum DhallElement {
    T(String),
    H(DhallHyperLink),
    Br,
    Img(DhallImage)
}
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, StaticType)]
enum DhallAddress {
    Plain(String),
    Fancy(Vec<DhallElement>),
}
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, StaticType)]
struct DhallAddressNick {
    #[serde(rename = "mapKey")]
    nick: String,
    #[serde(rename = "mapValue")]
    address: DhallAddress,
}

#[derive(Debug, PartialEq, Serialize, Deserialize, Clone, StaticType)]
struct DhallSignatureNick {
    #[serde(rename = "mapKey")]
    nick: String,
    #[serde(rename = "mapValue")]
    signature: Vec<DhallElement>,
}
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
struct DhallLetters {
    addresses: HashMap<String, DhallAddress>,
    signatures: HashMap<String,Vec<DhallElement>>
}

fn dhall_to_paras(elts:&Vec<DhallElement>) -> Vec<Paragraph> {
    let mut buf: Vec<Paragraph> = Vec::new();
    let mut para = Paragraph::new();
    for elt in elts {
        match elt {
            DhallElement::T(x) => { 
                let mut run = Run::new();
                run = run.add_text(format!("{} ",x)) ;
                para = para.add_run(run);
            }
            DhallElement::H(link) => { 
                let mut hl = Hyperlink::new(&link.href,HyperlinkType::External);
                hl = hl.add_run(Run::new().add_text(link.text.clone()).color("0000FF").underline("single"));
                para = para.add_hyperlink(hl);
            } 
            DhallElement::Br => {
                buf.push(para);
                para = Paragraph::new();
            }
            DhallElement::Img(image) => {
                let mut img = std::fs::File::open(&image.path).unwrap();
                let mut buf = Vec::new();
                let _ = img.read_to_end(&mut buf).unwrap();
                let pic = Pic::new(&buf);
                let (sizex, sizey) = pic.size;
                para = para.add_run(Run::new().add_image(pic.size((sizex as f64 * image.scale) as u32 , (sizey as f64 * image.scale) as u32)));
            }
        }
    }
    buf.push(para);
    buf
}

fn main() -> Result<(), Box<dyn std::error::Error>> {

    let clops = Args::parse();

    if clops.completion {
        generate(Bash, &mut Args::into_app(), "amkhlv-docx", &mut io::stdout());
        return Ok(())
    }

    let filename = format!("{}.docx",clops.output);
    let path = std::path::Path::new(&filename);
    let file = std::fs::File::create(&path).unwrap();

    let letters : DhallLetters = serde_dhall::from_file(std::path::Path::new(&home_dir().unwrap()).join("a/Dhall/letters.dhall")).parse().unwrap();
    println!("{:?}",letters);

    let mut run = Run::new();
    let lines = io::stdin().lines();
    let mut doc = Docx::new();

    if let Some(title) = clops.title { 
        let mut ttl_run = Run::new();
        ttl_run = ttl_run.bold();
        ttl_run = ttl_run.add_text(title).size(clops.size * 5 / 4);
        let ttl_paragraph = Paragraph::new().add_run(ttl_run).align(AlignmentType::Center);
        doc = doc.add_paragraph(ttl_paragraph);
    }
    if let Some(from) = clops.from {
        if let Some(addr) = letters.addresses.get(&from) {
            match addr {
                DhallAddress::Plain(x) => { 
                    doc = doc.add_paragraph(Paragraph::new().add_run(Run::new().add_text(x)).align(AlignmentType::Right));
                }
                DhallAddress::Fancy(elts) => {
                    for para in dhall_to_paras(elts) {
                        doc = doc.add_paragraph(para.align(AlignmentType::Right));
                    }
                }
            };
        }
    }
    if let Some(to) = clops.to {
        if let Some(addr) = letters.addresses.get(&to) {
            match addr {
                DhallAddress::Plain(x) => { 
                    doc = doc.add_paragraph(Paragraph::new().add_run(Run::new().add_text(x)).align(AlignmentType::Left));
                }
                DhallAddress::Fancy(elts) => {
                    for para in dhall_to_paras(elts) {
                        doc = doc.add_paragraph(para.align(AlignmentType::Left));
                    }
                }
            };
        }
    }
    if let Some(salutation) = clops.salutation {
            let p = Paragraph::new().add_run(Run::new().add_text(salutation)).align(AlignmentType::Left);
            doc = doc.add_paragraph(p);
    }

    for line in lines {
        run = run.add_text(line.unwrap()).size(clops.size);
    }

    doc = doc.add_paragraph(Paragraph::new().add_run(run));

    if let Some(signature) = clops.signature {
        if let Some(sig) = letters.signatures.get(&signature) {
            for para in dhall_to_paras(sig) {
                        doc = doc.add_paragraph(para.align(AlignmentType::Left));
                    }
        }
    }

    doc.build().pack(file)?;
    Ok(())
}
