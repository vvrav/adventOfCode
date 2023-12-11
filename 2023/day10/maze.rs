use std::env;
use std::fmt::Display;
use std::fs;
use std::slice::Iter;

#[derive(Copy, Clone, PartialEq)]
enum Direction {
    North,
    South,
    East,
    West,
}

impl Direction {
    fn coords(&self) -> (i32, i32) {
        match self {
            Self::East => (0, 1),
            Self::North => (-1, 0),
            Self::South => (1, 0),
            Self::West => (0, -1),
        }
    }

    fn next(&self, c: char) -> Self {
        match (c, self) {
            ('|', d) => *d,
            ('-', d) => *d,
            ('L', Self::South) => Self::East,
            ('L', Self::West) => Self::North,
            ('J', Self::South) => Self::West,
            ('J', Self::East) => Self::North,
            ('7', Self::North) => Self::West,
            ('7', Self::East) => Self::South,
            ('F', Self::North) => Self::East,
            ('F', Self::West) => Self::South,
            (c, d) => panic!("Not supposed to happen! ({}, {})", c, d),
        }
    }

    fn left(&self) -> Self {
        match self {
            Self::East => Self::North,
            Self::North => Self::West,
            Self::West => Self::South,
            Self::South => Self::East,
        }
    }

    fn values() -> Iter<'static, Direction> {
        static DIRECTIONS: [Direction; 4] = [
            Direction::North,
            Direction::South,
            Direction::East,
            Direction::West,
        ];
        DIRECTIONS.iter()
    }
}

impl Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::North => "North",
            Self::East => "East",
            Self::South => "South",
            Self::West => "West",
        };
        write!(f, "{}", s)
    }
}

fn beautify(c: char) -> char {
    match c {
        '7' => '┐',
        '|' => '│',
        'L' => '└',
        'J' => '┘',
        'F' => '┌',
        '-' => '─',
        c => c,
    }
}

fn parse_input(input: &String) -> (Vec<Vec<char>>, (i32, i32)) {
    let mut map: Vec<Vec<char>> = Vec::new();
    input
        .split('\n')
        .filter(|l| !l.is_empty())
        .for_each(|l| map.push(l.chars().collect()));
    let mut start = None;
    for (i, l) in (0..).zip(map.iter()) {
        match l.iter().position(|c| *c == 'S') {
            Some(j) => {
                start = Some((i, j as i32));
                break;
            }
            None => (),
        }
    }
    let start = start.expect("Start position not found");
    (map, start)
}

fn start_directions(map: &Vec<Vec<char>>, start: &(i32, i32)) -> (Direction, Direction) {
    let mut res: Vec<Direction> = Vec::new();
    let max_i = map.len() - 1;
    let max_j = map[0].len() - 1;
    let i = start.0 as usize;
    let j = start.1 as usize;
    if i > 0 && ['|', 'F', '7'].contains(&map[i - 1][j]) {
        res.push(Direction::North)
    }
    if j < max_j && ['7', '-', 'J'].contains(&map[i][j + 1]) {
        res.push(Direction::East)
    }
    if i < max_i && ['|', 'L', 'J'].contains(&map[i + 1][j]) {
        res.push(Direction::South)
    }
    if j > 0 && ['F', '-', 'L'].contains(&map[i][j - 1]) {
        res.push(Direction::West)
    }

    assert_eq!(res.len(), 2);
    (res[0], res[1])
}

fn get(map: &Vec<Vec<char>>, pos: &(i32, i32)) -> char {
    map[pos.0 as usize][pos.1 as usize]
}

fn run(map: &Vec<Vec<char>>, start: &(i32, i32)) -> (i32, Vec<Vec<char>>) {
    let (mut d1, mut d2) = start_directions(&map, &start);
    let mut p1 = start.clone();
    let mut p2 = start.clone();
    let mut step = 0;
    let mut clean_map = vec![vec![' '; map[0].len()]; map.len()];
    clean_map[start.0 as usize][start.1 as usize] = 'S';

    loop {
        println!(
            "step {}: {}, ({},{})  ;  {}, ({},{})",
            step, d1, p1.0, p1.1, d2, p2.0, p2.1
        );

        let (di, dj) = d1.coords();
        p1 = (p1.0 + di, p1.1 + dj);
        d1 = d1.next(get(&map, &p1));
        let (di, dj) = d2.coords();
        p2 = (p2.0 + di, p2.1 + dj);
        d2 = d2.next(get(&map, &p2));
        step += 1;

        // println!("step {}: {}, {:#?}  ;  {}, {:#?}", step, d1, p1, d2, p2);
        clean_map[p1.0 as usize][p1.1 as usize] = map[p1.0 as usize][p1.1 as usize];
        if p1 == p2 {
            break;
        }
        clean_map[p2.0 as usize][p2.1 as usize] = map[p2.0 as usize][p2.1 as usize];
    }

    (step, clean_map)
}

fn fill(map: &mut Vec<Vec<char>>, c: char, (i, j): (i32, i32)) {
    if i >= 0
        && i < map.len() as i32
        && j >= 0
        && j < map[0].len() as i32
        && map[i as usize][j as usize] == ' '
    {
        map[i as usize][j as usize] = c;
        for dir in Direction::values() {
            let (di, dj) = dir.coords();
            fill(map, c, (i + di, j + dj));
        }
    }
}

fn colorize(map: &Vec<Vec<char>>, start: (i32, i32), c: char) -> Vec<Vec<char>> {
    let mut map = map.clone();
    let (mut d, _) = start_directions(&map, &start);
    let (mut i, mut j) = start.clone();
    let (di, dj) = d.left().coords();
    fill(&mut map, c, (i + di, j + dj));
    loop {
        // advance!
        let (di, dj) = d.coords();
        i = i + di;
        j = j + dj;
        // color left
        let (di, dj) = d.left().coords();
        fill(&mut map, c, (i + di, j + dj));

        // reached the start ? STOP !
        if (i, j) == start {
            break;
        }
        // turn
        let nd = d.next(get(&map, &(i, j)));
        if nd != d {
            // if turned, color left again!
            let (di, dj) = nd.left().coords();
            fill(&mut map, c, (i + di, j + dj));
        }
        d = nd;
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

    let (map, start) = parse_input(&input);
    let (res, clean_map) = run(&map, &start);

    println!("STEP1: {}", res);

    println!("Cleaned map:");
    for l in clean_map.iter() {
        println!("{}", l.iter().map(|c| beautify(*c)).collect::<String>())
    }

    let colored_map = colorize(&clean_map, start, '*');

    println!("Colored map:");
    for l in colored_map.iter() {
        println!("{}", l.iter().map(|c| beautify(*c)).collect::<String>())
    }

    let count_blank: usize = colored_map
        .iter()
        .map(|l| l.iter().filter(|c| **c == ' ').count())
        .sum();
    let count_stars: usize = colored_map
        .iter()
        .map(|l| l.iter().filter(|c| **c == '*').count())
        .sum();

    println!(
        "STEP2:\n  left space: {}\n  right space: {}",
        count_stars, count_blank
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fill() {
        let mut input = vec![vec![' '; 3], vec![' ', 'x', ' '], vec![' '; 3]];
        let expected = vec![vec!['o'; 3], vec!['o', 'x', 'o'], vec!['o'; 3]];
        fill(&mut input, 'o', (0, 0));
        assert_eq!(input, expected)
    }

    #[test]
    fn test_colorize() {
        let input: Vec<Vec<char>> = vec![
            "     ".chars().collect(),
            " F-7 ".chars().collect(),
            " | | ".chars().collect(),
            " L-J ".chars().collect(),
            "     ".chars().collect(),
        ];
        let expected: Vec<Vec<char>> = vec![
            "     ".chars().collect(),
            " F-7 ".chars().collect(),
            " |*| ".chars().collect(),
            " L-J ".chars().collect(),
            "     ".chars().collect(),
        ];
        assert_eq!(colorize(&input, (3, 3), '*'), expected)
    }
}
