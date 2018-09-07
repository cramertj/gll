extern crate gll_lua;
use gll_lua::with_chunk;
use std::env;
use std::fs;
use std::time::Instant;

fn main() {
    let src = fs::read_to_string(env::args().nth(1).unwrap()).unwrap();
    for _ in 0..10 {
        let start = Instant::now();
        with_chunk(&src, |stats| {
            let end = Instant::now();
            println!("{:?}", end.duration_since(start));
            //let parser = stats.parser;

            // for stat in stats {
            //     let stat = stat.unwrap();
            //     println!("{:?}: {:?}", parser.input(stat.node.range), stat);
            // }
        });
    }
}
