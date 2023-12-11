use num::Integer;
use regex::Regex;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::env;
use std::fs;

type Node = (char, char, char);

fn node_of_str(str: &str) -> Node {
    let mut chars = str.chars();
    (
        chars.next().unwrap(),
        chars.next().unwrap(),
        chars.next().unwrap(),
    )
}

fn string_of_node(n: Node) -> String {
    [n.0, n.1, n.2].iter().collect()
}

fn map_of_input_lines(input: &Vec<&str>) -> BTreeMap<(Node, char), Node> {
    let mut map = BTreeMap::new();
    let re_line = Regex::new(r"^(\w+) = \((\w+), (\w+)\)$").unwrap();
    for line in input.iter().skip(1) {
        let cap = re_line.captures(line).unwrap();
        map.insert((node_of_str(&cap[1]), 'L'), node_of_str(&cap[2]));
        map.insert((node_of_str(&cap[1]), 'R'), node_of_str(&cap[3]));
    }
    map
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

    let mut dirs = lines[0].chars().cycle();
    let desert_map = map_of_input_lines(&lines);

    let mut current = node_of_str("AAA");
    let end = node_of_str("ZZZ");
    let mut step_count = 0u32;

    if desert_map.contains_key(&(current, 'L')) {
        while current != end {
            current = desert_map[&(current, dirs.next().unwrap())];
            step_count += 1;
        }
        println!("STEP1: {}", step_count);
    }

    let start: Vec<Node> = desert_map
        .keys()
        .filter_map(|key| match key {
            (n @ (_, _, 'A'), 'L') => Some(*n),
            _ => None,
        })
        .collect();

    let instrs: Vec<char> = lines[0].chars().collect();
    let n = instrs.len();
    let ends: Vec<Vec<usize>> = start
        .iter()
        .map(|s| {
            let mut acc: Vec<usize> = Vec::new();
            let mut set: BTreeSet<((char, char, char), usize)> = BTreeSet::new();
            let mut c = *s;
            for step in 0usize.. {
                let i = step % n;
                if !set.insert((c, i)) {
                    break;
                }
                if c.2 == 'Z' {
                    acc.push(step);
                }
                c = desert_map[&(c, instrs[i])];
            }
            acc
        })
        .collect();

    if ends.iter().all(|e| e.len() == 1) {
        // the easy way?
        println!("only one end in each cycle ? lcm?");
        let res = ends
            .iter()
            .map(|e| e[0])
            .reduce(|acc, e| acc.lcm(&e))
            .unwrap();

        println!("STEP2: {}", res);
    } else {
        // the hard way? wont work for the input...
        let mut currents = start;
        let mut dirs = lines[0].chars().cycle();
        let mut step_count = 0u32;
        while currents.iter().any(|(_, _, c)| *c != 'Z') {
            let d = dirs.next().unwrap();
            for c in currents.iter_mut() {
                *c = desert_map[&(*c, d)];
            }
            step_count += 1;
        }
        println!("STEP2: {}", step_count);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_map_of_input() {
        let input = vec![
            "LLR",
            "AAA = (BBB, BBB)",
            "BBB = (AAA, ZZZ)",
            "ZZZ = (ZZZ, ZZZ)",
        ];
        let output = map_of_input_lines(&input);
        assert_eq!(output[&(node_of_str("AAA"), 'L')], node_of_str("BBB"));
        assert_eq!(output[&(node_of_str("BBB"), 'R')], node_of_str("ZZZ"));
    }
}
