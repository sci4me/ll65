use clap::*;
use std::fs;
use std::process;

use ll65::asm::textual::lexer::Lexer;
use ll65::asm::textual::lexer::Span;

fn main() {
    let matches = App::new(crate_name!())
        .version(crate_version!())
        .author(crate_authors!())
        .about(crate_description!())
        .arg(
            Arg::with_name("file")
                .takes_value(true)
                .value_name("FILE")
                .required(true)
                .help("The input source file to compile/assemble"),
        )
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
        println!(
            "{:?}\n{:?}",
            lexer.get_token(),
            lexer.get_line(lexer.get_token().span.start)
        );
        lexer.eat_token().unwrap();
    }

    let lines = lexer.get_lines_in_span(Span::new(114, 134))
        .iter()
        .fold(String::new(), |sum, curr| {
            if sum == "" {
                curr.clone()
            } else {
                sum + "\n" + curr
            }
        });
    println!("{}", lines);
}
