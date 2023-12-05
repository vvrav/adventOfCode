use std::collections::BTreeSet;
use std::env;
use std::fs;

fn process_card(line: &str) -> (usize, usize) {
    let (prefix, content) = line.split_once(':').unwrap();
    let game: usize = prefix.strip_prefix("Card").unwrap().trim().parse().unwrap();
    let (winning_str, numbers_str) = content.split_once('|').unwrap();

    let winning_set = BTreeSet::from_iter(
        winning_str
            .split_whitespace()
            .map(|s| s.parse::<usize>().unwrap()),
    );
    let numbers_set = BTreeSet::from_iter(
        numbers_str
            .split_whitespace()
            .map(|s| s.parse::<usize>().unwrap()),
    );

    (game, winning_set.intersection(&numbers_set).count())
}

fn get_score(n: usize) -> u32 {
    if n > 0 {
        2u32.pow(u32::try_from(n).unwrap() - 1)
    } else {
        0
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = match args.get(1) {
        Some(s) => s,
        None => "example",
    };

    println!("{}", filename);

    let input = fs::read_to_string(filename).expect("Failed to read input");
    let lines: Vec<_> = input.split('\n').filter(|l| !l.is_empty()).collect();

    let processed_lines: Vec<(usize, usize)> = lines.iter().map(|l| process_card(&l)).collect();

    let score = processed_lines
        .iter()
        .fold(0, |acc, (_, e)| acc + get_score(*e));

    println!("STEP1: {}", score);

    let mut count = vec![1u32; lines.len()];
    processed_lines.iter().for_each(|(game, n)| {
        for i in *game..(*game + *n) {
            count[i] = count[i] + count[*game - 1];
        }
    });
    println!("STEP2: {}", count.iter().sum::<u32>());
}
