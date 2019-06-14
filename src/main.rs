use clap::*;
use std::fs;
use std::process;

use ll65::asm::textual::lexer::Lexer;

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

    let mut lexer = Lexer::new(file.to_string(), contents).unwrap();

    while lexer.has_token() {
        println!(
            "{:?}\n{:?}",
            lexer.get_token(),
            lexer.get_line(lexer.get_token().span.start)
        );

        // fuck the police

        match lexer.eat_token() {
            Ok(_) => {},
            Err(e) => {
                eprintln!("\u{001b}[31m\u{001b}[1merror:\u{001b}[37m {}\u{001b}[0m", e);
                eprintln!("   \u{001b}[34;1m-->\u{001b}[0m {}", file);
                let span = lexer.span();
                let lines = (span.start..span.end)
                    .into_iter()
                    .map(|i| (lexer.index_to_line(i), i))
                    .collect::<Vec<(u32, usize)>>();
                
                let lines = {
                    let mut index = 0;
                    let mut last = lines[index];
                    let mut result = String::new();
                    while index < lines.len() {
                        let curr = lines[index];
                        if curr.0 != last.0 {
                            result = format!("{}\n  \u{001b}[34m;1m{} |\u{001b}[0m {}", result, curr.0, lexer.get_line(curr.1));
                        } else if result.is_empty() {
                            result = format!(" \u{001b}[34;1m{} |\u{001b}[0m {}", curr.0, lexer.get_line(curr.1));
                        }
                        index += 1;
                        last = curr;
                    }
                    result
                };

                // let lines = (span.start..span.end)
                //     .into_iter()
                //     .map(|i| (i, lexer.get_line(i)))
                //     .map(|(i, line)| (i, format!("    \u{001b}[34;1m{} |\u{001b}[0m{}", lexer.index_to_line(i), line)))
                //     .fold(None, |sum, (i, line)| {
                //         if let Some(sum) = sum {
                //             Some(format!("{}\n{}", sum, line))
                //         } else {
                //             Some(format!("   \u{001b}[34;1m{} |\u{001b}[0m {}", lexer.index_to_line(i), line))
                //         }
                //     })
                //     .unwrap();

                eprintln!("    \u{001b}[34;1m|\u{001b}[0m");
                eprintln!("{}", lines);
                eprintln!("    \u{001b}[34;1m|\u{001b}[0m");
            }
        }
    }
}
