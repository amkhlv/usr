use clap::{Parser,IntoApp};
use std::process::Command;
use clap_complete::{generate, shells::Bash};
use std::io;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
#[clap(author, 
       version, 
       about = "
        print HTML files to PDF using wkhtml2pdf
        ",
       long_about = None)]
struct Args {
    /// Specify the output file
    #[clap(short, long, value_name="OUTPUT_PDF_FILE")]
    output: Option<String>,

    #[clap(long, default_value_t = String::from("30mm"))]
    margin_top: String,

    #[clap(long, default_value_t = String::from("30mm"))]
    margin_bottom: String,

    #[clap(long, default_value_t = String::from("20mm"))]
    margin_left: String,

    #[clap(long, default_value_t = String::from("20mm"))]
    margin_right: String,
    
    /// Generate bash completion
    #[clap(long)]
    completion: bool,

    #[clap(value_name="INPUT_HTML_FILE_OR_URL")]
    input: String

}
fn main() -> Result<(), Box<dyn std::error::Error>> {

    let clops = Args::parse();
    
    if clops.completion {
        generate(Bash, &mut Args::into_app(), "amkhlv-wkhtml2pdf", &mut io::stdout());
        return Ok(())
    }

    let status = Command::new("wkhtmltopdf")
        .args([ "--margin-bottom"
              , &clops.margin_bottom
              , "--margin-top"
              , &clops.margin_top
              , "--margin-left"
              , &clops.margin_left
              , "--margin-right"
              , &clops.margin_right
              , "--enable-local-file-access"
              , &clops.input 
              , &clops.output.unwrap_or(clops.input.clone() + ".pdf")
        ])
        .status()? ;

    println!("{}",status);

    Ok(())
}
