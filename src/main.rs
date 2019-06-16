use clap::*;
use std::fs;
use std::process;

use ll65::asm::textual::lexer::Lexer;
use ll65::asm::textual::parser::Parser;

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

    let lexer = Lexer::new(file.to_string(), contents).unwrap();
    let mut parser = Parser::new(lexer);

    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    };

    // while lexer.has_token() {
    //     // println!(
    //     // "{:?}\n{:?}",
    //     // lexer.get_token(),
    //     // lexer.get_line(lexer.get_token().span.start)
    //     // );
    //     println!("{:?}", lexer.get_token());
    //     // println!("{:?}", lexer.get_line(lexer.get_token().span.start));

    //     match lexer.eat_token() {
    //         Ok(_) => {}
    //         Err(e) => {
    //             eprintln!("{}", lexer.format_error_message(e));
    //             std::process::exit(1);
    //         }
    //     }
    // }
}
