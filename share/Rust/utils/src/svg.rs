use clap::{Parser,IntoApp};
use std::process::Command;
use std::collections::HashMap;
use std::path::{PathBuf,Path};
use clap_complete::{generate, shells::Bash};
use std::{io,fs};
use home::home_dir;
use std::fs::File;
use std::process::Stdio;
use regex::Regex;
use steel::steel_vm::engine::Engine;
use steel::rvals::SteelVal;

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
    /// SVG file name
    #[clap(short, long, value_name="new file name (without \".svg\")")]
    new: Option<String>,

    /// Template
    #[clap(short,long, value_name="template file")]
    template: Option<String>,

    /// Staple to single PDF file (provide SVGs as arguments)
    #[clap(long,value_name="PDF_FILE")]
    to_pdf: Option<String>,

    /// burst PDF to SVGs (provide input PDF file as a single argument)    
    #[clap(long,value_name="SVG_COLLECTION_DIR")]
    to_svg: Option<String>,

        /// name transformer, see pdf2svg manual, e.g. (lambda (x) (string-append x "_%03d.svg"))
    #[clap(long,value_name="NAME_TRANSFORMER")]
    transformer: Option<String>,

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

        let template = if clops.template.is_none() {
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
            hints.get(num_buf.trim()).expect("index out of range").path()
        } else {
            let mut pb = PathBuf::new(); pb.push(&clops.template.unwrap()); pb
        };

        if let Some(newfile) = clops.new {
            let xmltrans = Command::new("xmlstarlet")
                .args([ "ed"
                      , "-N"
                      , "xmlns=http://www.w3.org/2000/svg"
                      , "-u"
                      , "//xmlns:svg/@sodipodi:docname"
                      , "-v"
                      , (newfile.clone() + ".svg").as_ref()
                      , template.to_str().unwrap()
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
            let in_path_str : &str = clops.input.iter().next().unwrap();
            let in_path : &Path = Path::new(in_path_str);
            let in_filename = in_path.file_name().unwrap();
            let out_filename : String = if let Some(trans) = clops.transformer {
                let mut steel_vm = Engine::new();
                match steel_vm.run(format!("({} \"{}\")", trans, in_filename.to_str().unwrap())) {
                    Ok(mut v) => {
                        match v.pop().expect("transformer returned empty Vec") {
                          SteelVal::StringV(x) => x.to_string(),
                          steel_value => panic!("transformer returned not String, but: {:?}", steel_value)  
                        }
                    }
                    Err(e) => {
                        panic!("Steel error: {:?}",e);
                    }
                }
            } else {
                Regex::new(".pdf$")
                .unwrap()
                .replace(
                    in_filename.to_str().unwrap(),
                    clops.page.map(|pg| { format!("_p{:03}.svg",pg) }).unwrap_or("_%03d.svg".to_string()) 
                    )
                .to_string()
            };
            let out_path = Path::new(&outdir).join(out_filename);
            let mut args = vec![in_path_str, out_path.to_str().unwrap()];
            let pgarg = if let Some(pg) = clops.page { pg.to_string() } else { String::from("all") };
            args.push(&pgarg);
            println!("{} {:?}", "pdf2svg", &args);
            let pdftosvg = Command::new("pdf2svg").args(args).status();
            println!("pdf2svg: {:?}",pdftosvg);
            return Ok(());
        }
    }

    Ok(())
}
