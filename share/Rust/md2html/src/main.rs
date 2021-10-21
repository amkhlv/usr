use markdown::file_to_html;
use std::path::Path;
use std::env::args;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = args().collect();
    let result = file_to_html(Path::new(&args[1]))?;
    println!("{}",result);
    Ok(())
}
