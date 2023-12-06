use regex::Regex;
use std::env;
use std::fs;

fn process_input(input: &Vec<&str>) -> (Vec<u32>, Vec<(String, String, Vec<(u32, u32, u32)>)>) {
    let seeds: Vec<u32> = input[0]
        .strip_prefix("seeds:")
        .unwrap()
        .split_whitespace()
        .map(|s| s.parse::<u32>().unwrap())
        .collect();

    let map_regex = Regex::new(r"^([a-z]+)-to-([a-z]+) map:").unwrap();
    let recipes: Vec<(String, String, Vec<(u32, u32, u32)>)> = input[2..]
        .split(|l| l.trim().is_empty())
        .filter(|&block| !block.is_empty()) // remove empty blocks (ie last line if any)
        .map(|lines| {
            let capt = map_regex.captures(lines[0]).unwrap();
            let from = capt[1].to_owned();
            let to = capt[2].to_owned();
            let ranges: Vec<(u32, u32, u32)> = lines
                .iter()
                .skip(1)
                .map(|l| {
                    let nums: Vec<u32> = l
                        .split_whitespace()
                        .map(|s| s.parse::<u32>().unwrap())
                        .collect();
                    (nums[0], nums[1], nums[2])
                })
                .collect();
            (from, to, ranges)
        })
        .collect();
    (seeds, recipes)
}

fn num_map(n: u32, ranges: &Vec<(u32, u32, u32)>) -> u32 {
    for (dest, source, range) in ranges {
        if n >= *source && n < source + range {
            return dest + n - source;
        }
    }
    n
}

fn range_map(ranges: &Vec<(u32, u32)>, recipes: &Vec<(u32, u32, u32)>) -> Vec<(u32, u32)> {
    let mut remaining = ranges.clone();
    let mut result = Vec::new();

    for (dest, source, range) in recipes {
        let mut new_rem = Vec::new();
        let rec_start = *source;
        let rec_end = source + range;
        remaining.iter().for_each(|(start, size)| {
            let seed_start = *start;
            let seed_end = start + size;
            if rec_start >= seed_end || rec_end <= seed_start {
                // nothing to do with this interval
                new_rem.push((seed_start, *size));
                return;
            }
            if rec_start > seed_start && rec_end < seed_end {
                // the whole recipe is matched, not the whole seed is consumed
                new_rem.push((seed_start, rec_start - seed_start));
                result.push((*dest, *range));
                new_rem.push((rec_end, seed_end - rec_end));
                return;
            }
            if rec_end < seed_end {
                // first half is matched
                result.push((dest + (seed_start - rec_start), rec_end - seed_start));
                new_rem.push((rec_end, seed_end - rec_end));
                return;
            }
            if rec_start > seed_start {
                // second half is matched
                new_rem.push((seed_start, rec_start - seed_start));
                result.push((*dest, seed_end - rec_start));
                return;
            }
            // last case: the whole seed interval is matched?
            result.push((seed_start - rec_start + dest, *size));
        });
        remaining = new_rem;
    }
    for e in remaining {
        result.push(e)
    }
    result
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

    let (init_seeds, recipes) = process_input(&lines);

    let mut current = &"seed".to_string();
    let mut seeds = init_seeds.clone();

    while current != "location" {
        let (_, to, ranges) = recipes.iter().find(|(from, _, _)| from == current).unwrap();
        current = to;
        seeds = seeds.iter().map(|s| num_map(*s, ranges)).collect();
    }

    let min = seeds.iter().min().unwrap();
    println!("STEP1: {}", min);

    let mut pairs: Vec<(u32, u32)> = init_seeds.chunks_exact(2).map(|c| (c[0], c[1])).collect();
    let mut current = &"seed".to_string();

    while current != "location" {
        let (_, to, ranges) = recipes.iter().find(|(from, _, _)| from == current).unwrap();
        current = to;
        pairs = range_map(&pairs, ranges);
    }
    let min = pairs.iter().map(|(s, _)| s).min().unwrap();
    println!("STEP2: {}", min);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process_input() {
        let input_lines = vec!["seeds: 1 2 3 4", "", "foo-to-bar map:", "1 2 3", "4 5 6"];
        let expected: (Vec<u32>, Vec<(String, String, Vec<(u32, u32, u32)>)>) = (
            vec![1, 2, 3, 4],
            vec![(
                "foo".to_string(),
                "bar".to_string(),
                vec![(1, 2, 3), (4, 5, 6)],
            )],
        );
        let output = process_input(&input_lines);
        assert_eq!(output, expected);
    }

    #[test]
    fn test_num_map() {
        let ranges = vec![(50, 98, 2), (52, 50, 48)];
        assert_eq!(num_map(79, &ranges), 81);
        assert_eq!(num_map(14, &ranges), 14);
        assert_eq!(num_map(55, &ranges), 57);
        assert_eq!(num_map(13, &ranges), 13);
    }

    #[test]
    fn test_range_map() {
        let recipes = vec![(50, 98, 2), (52, 50, 48)];

        // one range out of recipes
        assert_eq!(range_map(&vec![(10, 10)], &recipes), vec![(10, 10)]);

        // one range included in recipe
        assert_eq!(range_map(&vec![(60, 10)], &recipes), vec![(62, 10)]);

        // example from aoc
        let ranges = vec![(79, 14), (55, 13)];
        let expected = vec![(81, 14), (57, 13)];
        assert_eq!(range_map(&ranges, &recipes), expected)
    }
}
