use itertools::Itertools;
use std::env;
use std::fs;

fn parse_input(input: &Vec<&str>) -> Vec<(u64, u64)> {
    let times = input[0]
        .split_whitespace()
        .skip(1)
        .map(|s| s.parse::<u64>().unwrap());
    let distances = input[1]
        .split_whitespace()
        .skip(1)
        .map(|s| s.parse::<u64>().unwrap());

    times.zip(distances).collect()
}

/// return interval of winning pressure time (bounds included)
fn process_race((time, distance): &(u64, u64)) -> (u64, u64) {
    let delta = time * time - 4 * distance;
    let sqd = (delta as f64).sqrt();
    let x1 = ((*time as f64 - sqd) / 2f64) as u64;
    let x2 = ((*time as f64 + sqd) / 2f64) as u64;
    let min = if x1 * (time - x1) > *distance {
        x1
    } else {
        x1 + 1
    };
    let max = if x2 * (time - x2) > *distance {
        x2
    } else {
        x2 - 1
    };
    (min, max)
}

fn parse_input_for_step2(input: &Vec<&str>) -> (u64, u64) {
    let time = input[0]
        .split_whitespace()
        .skip(1)
        .join("")
        .parse()
        .unwrap();
    let distance = input[1]
        .split_whitespace()
        .skip(1)
        .join("")
        .parse()
        .unwrap();

    (time, distance)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = match args.get(1) {
        Some(s) => s,
        None => "example",
    };

    println!("{}", filename);

    let input = fs::read_to_string(filename).expect("Failed to read input");
    let lines: Vec<_> = input.split('\n').collect();

    let races = parse_input(&lines);
    let res1: u64 = races
        .iter()
        .map(process_race)
        .map(|(x, y)| y - x + 1)
        .product();

    println!("STEP1: {}", res1);
    let (min, max) = process_race(&parse_input_for_step2(&lines));
    let ways = max - min + 1;
    println!("STEP2: {}", ways);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = vec!["Time:      7  15   30", "Distance:  9  40  200"];
        let expected = vec![(7, 9), (15, 40), (30, 200)];
        assert_eq!(parse_input(&input), expected);
    }

    #[test]
    fn test_race() {
        assert_eq!(process_race(&(7, 9)), (2, 5));
        assert_eq!(process_race(&(15, 40)), (4, 11));
        assert_eq!(process_race(&(30, 200)), (11, 19));
        // test aux limites: les racines sont des entiers
        assert_eq!(process_race(&(4, 3)), (2, 2));
    }
}
