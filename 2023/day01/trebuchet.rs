use regex::Regex;
use std::env;
use std::fs;

fn process_line(line: &str) -> Option<u32> {
    let first = line.chars().find(|c| c.is_ascii_digit());
    let last = line.chars().rev().find(|c| c.is_ascii_digit());
    match (first, last) {
        (Some(f), Some(l)) => Some(10 * f.to_digit(10).unwrap() + l.to_digit(10).unwrap()),
        _ => None,
    }
}

fn replacer(s: &str) -> u32 {
    match s {
        "one" => 1,
        "two" => 2,
        "three" => 3,
        "four" => 4,
        "five" => 5,
        "six" => 6,
        "seven" => 7,
        "eight" => 8,
        "nine" => 9,
        &_ => s.parse().unwrap(),
    }
}

fn process_line2(line: &str) -> u32 {
    let pattern = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", r"\d",
    ]
    .join("|");
    let regex = Regex::new(&pattern).unwrap();

    let first = &regex.captures(line).unwrap()[0];
    let mut last = None;
    // yuk :O
    for index in (0..line.len()).rev() {
        match regex.captures_at(line, index) {
            Some(m) => {
                let l = m[0].to_owned();
                last = Some(l);
                break;
            }
            None => (),
        }
    }
    let last = &last.unwrap();
    //println!("line: {}, found {}, {}", line, first, last);

    replacer(first) * 10 + replacer(last)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = match args.get(1) {
        Some(s) => s,
        None => "example",
    };

    println!("{}", filename);

    let input = fs::read_to_string(filename).expect("Failed to read input");
    let lines: Vec<&str> = input.split('\n').filter(|l| !l.is_empty()).collect();

    let ns: Vec<_> = lines.iter().map(|l| process_line(l)).collect();
    if ns.contains(&None) {
        println!("STEP1: input contain a line with no digit!")
    } else {
        let res = ns.into_iter().fold(0, |acc, e| acc + e.unwrap());
        println!("STEP1: The result: {}", res);
    }

    let ns = lines.iter().map(|l| process_line2(l));
    let res2 = ns.reduce(|acc, e| acc + e).expect("should not be empty!!!");

    println!("STEP2: The result: {}", res2);
}
