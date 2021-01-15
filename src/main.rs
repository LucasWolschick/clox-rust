use std::{fs::File, path::Path};

fn main() {
    let args: Vec<_> = std::env::args().skip(1).collect();
    match args.len() {
        0 => {
            clox::repl();
        }
        1 => {
            let path = Path::new(&args[0]);
            let file = File::open(path);
            match file {
                Ok(file) => {
                    clox::file(file);
                }
                Err(err) => {
                    eprintln!("Invalid path supplied: {}", err);
                    eprintln!("Usage: clox [path]");
                    std::process::exit(66);
                }
            };
        }
        _ => {
            eprintln!("Usage: clox [path]");
            std::process::exit(64);
        }
    }
}
