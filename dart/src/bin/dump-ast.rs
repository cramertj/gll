extern crate dart;
use gll_lua::with_chunk;

use std::env;
use std::fs;

fn main() {
    let files = WalkDir::new(env::args().nth(1).unwrap())
        .contents_first(true)
        .into_iter()
        .map(|entry| entry.unwrap())
        .filter(|entry| entry.path().extension().map_or(false, |ext| ext == "dart"));

    for file in files {
        let path = file.into_path();
        let src = match fs::read_to_string(&path) {
            Ok(x) => x,
            Err(e) => {
                eprintln!("{}: error reading file: {:?}", path.display(), e);
                continue;
            }
        };
        parse::LibraryDefinition::parse_with(&src[..], |_, result| match result {
            Ok(_) => {
                println!("{}: file parsed", path.display());
            }
            Err(e) => eprintln!("{}: error parsing with gll: {:?}", path.display(), e),
        });
    }
}
