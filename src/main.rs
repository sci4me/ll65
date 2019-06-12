use std::fs;
use std::process;
use clap::*;

use ll65::asm::textual::Lexer;

fn main() {
    let matches = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .about(crate_description!())
        .arg(Arg::with_name("file")
            .takes_value(true)
            .value_name("FILE")
            .required(true)
            .help("The input source file to compile/assemble"))
        .get_matches();

    let file = matches.value_of("file").expect("Internal Error");

    let contents = match fs::read_to_string(file) {
        Ok(s) => s,
        Err(_) => {
            eprintln!("Unable to read file: {}", file);
            process::exit(1);
        }
    };

    let mut lexer = Lexer::new(contents).unwrap();

    while lexer.has_token() {
        println!("{:?}", lexer.get_token());
        lexer.eat_token().unwrap();
    }
}