use clap::{Parser,IntoApp};
use std::process::Command;
use std::collections::HashMap;
use std::path::Path;
use clap_complete::{generate, shells::Bash};
use std::{io,fs};
use home::home_dir;
use std::fs::File;
use std::process::Stdio;
use regex::Regex;

#[derive(Parser, Debug)]
#[clap(author, 
       version, 
       about = "
        SVG staff
        templates should be in ~/.config/inkscape/templates/ 
        sudo aptitude install xmlstarlet
       ",
       long_about = None)]
struct Args {
    /// Specify the output file
    #[clap(short, long, value_name="new file name (without \".svg\")")]
    new: Option<String>,

    /// Staple to single PDF file (provide SVGs as arguments)
    #[clap(long,value_name="PDF_FILE")]
    to_pdf: Option<String>,

    /// burst PDF to SVGs (provide input PDF file as a single argument)    
    #[clap(long,value_name="SVG_COLLECTION_DIR")]
    to_svg: Option<String>,

    /// process one page only
    #[clap(long)]
    page: Option<u16>,

    /// Generate bash completion
    #[clap(long)]
    completion: bool,    

    /// Input (one PDF file or many SVG files)
    #[clap(value_name="INPUT_FILES")]
    input: Vec<String>

}
fn main() -> Result<(), Box<dyn std::error::Error>> {

    let clops = Args::parse();

    if clops.completion {
        generate(Bash, &mut Args::into_app(), "amkhlv-svg", &mut io::stdout());
        return Ok(())
    }

    if clops.new.is_some() {
        let mut templates_dir = home_dir().unwrap();
        templates_dir.push(".config/inkscape/templates");
        let templates = fs::read_dir(templates_dir).unwrap();
        let mut hints = HashMap::new();

        let mut i = 0;
        for template in templates {
            i = i + 1;
            println!("{}. {}", i, template.as_ref().unwrap().path().file_name().unwrap().to_str().unwrap());
            hints.insert(format!("{}",i),template.unwrap());
        };
        let mut num_buf = String::new();
        std::io::stdin().read_line(&mut num_buf).expect("should enter number");
        println!("{:?}", hints.get(num_buf.trim()));

        if let (Some(newfile), Some(template)) = (clops.new, hints.get(num_buf.trim())) {
            let xmltrans = Command::new("xmlstarlet")
                .args([ "ed"
                      , "-N"
                      , "xmlns=http://www.w3.org/2000/svg"
                      , "-u"
                      , "//xmlns:svg/@sodipodi:docname"
                      , "-v"
                      , (newfile.clone() + ".svg").as_ref()
                      , template.path().to_str().unwrap()
                ])
                .stdout(Stdio::from(File::create(newfile + ".svg").unwrap()))
                .status();
            println!("{:?}",xmltrans);
            return Ok(());
        }        
    };
    if let Some(output) = clops.to_pdf {
        for svgfile in &clops.input {
            let inkscape = Command::new("inkscape")
                .args([ "-o"
                      , &(svgfile.clone() + ".pdf")
                      , &svgfile
                ])
                .status();
                println!("Inkscape: {:?}",inkscape)
        }
        let pdftk = Command::new("pdftk")
            .args([clops.input.iter().map(|x| { x.to_owned() + ".pdf" }).collect(), vec!["cat".to_string(), "output".to_string(), output ]].concat())
            .status();
        println!("pdftk: {:?}",pdftk);
        return Ok(());
    } else {
        if let Some(outdir) = clops.to_svg {
            let in_filename : &str = clops.input.iter().next().unwrap();
            let out_filename : String = Regex::new(".pdf$")
                .unwrap()
                .replace(
                    in_filename,
                    clops.page.map(|pg| { format!("_p{}",pg) }).unwrap_or("_%03d.svg".to_string()) 
                    )
                .to_string();
            let out_path = Path::new(&outdir).join(out_filename);
            let mut args = vec![in_filename, out_path.to_str().unwrap()];
            let pgarg = if let Some(pg) = clops.page { pg.to_string() } else { String::from("all") };
            args.push(&pgarg);
            let pdftosvg = Command::new("pdf2svg").args(args).status();
            println!("pdf2svg: {:?}",pdftosvg);
            return Ok(());
        }
    }

    Ok(())
}
