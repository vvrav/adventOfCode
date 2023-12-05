use regex::Regex;
use std::env;
use std::fs;

fn get_part_numbers(schematic: &Vec<&str>) -> Vec<u32> {
    let re_num = Regex::new(r"\d+").unwrap();
    let re_spe = Regex::new(r"[^\s\d.]").unwrap();
    let line_len = schematic[0].chars().count();
    let mut res: Vec<u32> = Vec::new();

    schematic.iter().enumerate().for_each(|(i, line)| {
        let num_pos = re_num.find_iter(line);
        num_pos
            .filter(|m| {
                let start = m.start();
                let end = m.end();
                let min_x = if start > 0 { start - 1 } else { 0 };
                let max_x = if end >= line_len { line_len } else { end + 1 };
                (i > 0 && re_spe.is_match(&(schematic[i - 1])[min_x..max_x]))
                    || re_spe.is_match(&line[min_x..max_x])
                    || (i < schematic.len() - 1
                        && re_spe.is_match(&(schematic[i + 1])[min_x..max_x]))
            })
            .for_each(|m| res.push(line[m.start()..m.end()].parse::<u32>().unwrap()));
    });
    res
}

fn get_gear_ratios(schematic: &Vec<&str>) -> Vec<u32> {
    let re_num = Regex::new(r"\d+").unwrap();
    let re_gear = Regex::new(r"\*").unwrap();
    let mut gears = Vec::new();

    schematic.iter().enumerate().for_each(|(i, line)| {
        re_gear.find_iter(line).for_each(|m| {
            let pos = m.start();
            let mut matches: Vec<_> = re_num
                .find_iter(line)
                .map(|m| (i, m.start(), m.end()))
                .collect();
            if i > 0 {
                re_num
                    .find_iter(&schematic[i - 1])
                    .for_each(|m| matches.push((i - 1, m.start(), m.end())));
            }
            if i < schematic.len() - 1 {
                re_num
                    .find_iter(&schematic[i + 1])
                    .for_each(|m| matches.push((i + 1, m.start(), m.end())));
            }

            let mut labels: Vec<u32> = Vec::new();
            matches.iter().for_each(|(l, s, e)| {
                if pos + 1 >= *s && pos <= *e {
                    labels.push(schematic[*l][*s..*e].parse::<u32>().unwrap())
                }
            });

            if labels.len() == 2 {
                gears.push(labels[0] * labels[1]);
            }
        })
    });
    gears
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

    let nums = get_part_numbers(&lines);
    let sum = nums.iter().fold(0, |acc, e| acc + e);
    println!("STEP1: {}", sum);

    let gears = get_gear_ratios(&lines);
    let sum2 = gears.iter().fold(0, |acc, e| acc + e);
    println!("STEP2: {}", sum2);
}
