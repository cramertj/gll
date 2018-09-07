extern crate gll_lua;
extern crate luster;
extern crate walkdir;
use gll_lua::check_luster::Check;
use gll_lua::parse;
use luster::io::buffered_read;
use luster::parser::parse_chunk;
use std::env;
use std::fs;
use std::thread;
use walkdir::WalkDir;

fn main() {
    thread::Builder::new()
        .stack_size(32 * 1024 * 1024)
        .spawn(|| {
            let files = WalkDir::new(env::args().nth(1).unwrap())
                .contents_first(true)
                .into_iter()
                .map(|entry| entry.unwrap())
                .filter(|entry| entry.path().extension().map_or(false, |ext| ext == "lua"));

            for file in files {
                let path = file.into_path();
                let src = match fs::read_to_string(&path) {
                    Ok(x) => x,
                    Err(e) => {
                        eprintln!("{}: error reading file: {:?}", path.display(), e);
                        continue;
                    }
                };
                if src.len() >= 8 * 1024 {
                    eprintln!("skipped {}", path.display());
                    continue;
                }
                let luster_chunk = parse_chunk(buffered_read(src.as_bytes()).unwrap());
                match luster_chunk {
                    Ok(luster_chunk) => {
                        parse::Chunk::parse_with(&src[..], |_, result| match result {
                            Ok(chunk) => {
                                if let Err(e) = chunk.check(&luster_chunk) {
                                    eprintln!("{}: check failed: {:?}", path.display(), e);
                                }
                            }
                            Err(e) => {
                                eprintln!("{}: error parsing with gll: {:?}", path.display(), e)
                            }
                        })
                    }
                    Err(e) => eprintln!("{}: error parsing with luster: {:?}", path.display(), e),
                }
            }
        })
        .unwrap()
        .join()
        .unwrap();
}
