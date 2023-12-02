use regex::Regex;
use std::env;
use std::fs;

fn parse_subset(subset: &str) -> (u32, u32, u32) {
    let sr_blue = Regex::new(r"(\d+) blue").unwrap();
    let sr_green = Regex::new(r"(\d+) green").unwrap();
    let sr_red = Regex::new(r"(\d+) red").unwrap();
    let blue: u32 = sr_blue
        .captures(subset)
        .map_or(0, |c| c[1].parse::<u32>().unwrap());
    let red: u32 = sr_red
        .captures(subset)
        .map_or(0, |c| c[1].parse::<u32>().unwrap());
    let green: u32 = sr_green
        .captures(subset)
        .map_or(0, |c| c[1].parse::<u32>().unwrap());
    (red, green, blue)
}

fn parse_line(line: &str) -> (u32, Vec<(u32, u32, u32)>) {
    let (prefix, content) = line.split_once(':').unwrap();
    let game: u32 = prefix.strip_prefix("Game ").unwrap().parse().unwrap();
    (game, content.split(';').map(parse_subset).collect())
}

fn is_possible(
    max_red: u32,
    max_green: u32,
    max_blue: u32,
) -> impl Fn(&(u32, Vec<(u32, u32, u32)>)) -> Option<u32> {
    move |game: &(u32, Vec<(u32, u32, u32)>)| -> Option<u32> {
        let (id, subsets) = game;
        subsets
            .iter()
            .all(|(red, green, blue)| *red <= max_red && *green <= max_green && *blue <= max_blue)
            .then_some(*id)
    }
}

fn get_fewest_numbers((_, subsets): &(u32, Vec<(u32, u32, u32)>)) -> (u32, u32, u32) {
    subsets
        .iter()
        .fold(
            (0, 0, 0),
            |(max_red, max_gre, max_blu), (red, green, blue)| {
                (
                    if *red > max_red { *red } else { max_red },
                    if *green > max_gre { *green } else { max_gre },
                    if *blue > max_blu { *blue } else { max_blu },
                )
            },
        )
        .to_owned()
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = match args.get(1) {
        Some(s) => s,
        None => "example",
    };

    println!("{}", filename);

    let input = fs::read_to_string(filename).expect("Failed to read input");
    let lines = input.split('\n').filter(|l| !l.is_empty());
    let games: Vec<_> = lines.map(parse_line).collect();

    let res1 = games
        .iter()
        .map(is_possible(12, 13, 14))
        .fold(0, |acc, oid| oid.map_or(acc, |id| acc + id));
    println!("STEP1: {}", res1);

    let res2 = games
        .iter()
        .map(get_fewest_numbers)
        .fold(0, |acc, (r, g, b)| acc + r * g * b);
    println!("STEP2: {}", res2);
}
