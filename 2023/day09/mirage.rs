use std::env;
use std::fs;

fn print_tab(tab: &Vec<Vec<i32>>) {
    for i in 0..tab.len() {
        for e in tab[i].iter() {
            print!("{:3}", *e);
        }
        println!();
    }
}

fn process_line(line: &str) -> Vec<Vec<i32>> {
    let l: Vec<i32> = line
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect();
    let mut tab: Vec<Vec<i32>> = vec![l];
    while tab.last().unwrap().iter().any(|e| *e != 0) {
        let l = tab.last().unwrap();
        let mut nl = Vec::new();
        for i in 0..l.len() - 1 {
            nl.push(l[i + 1] - l[i])
        }
        tab.push(nl);
    }

    for i in (0..(tab.len()) - 1).rev() {
        let nval = tab[i].last().unwrap() + tab[i + 1].last().unwrap();
        let nval2 = tab[i][0] - tab[i + 1][0];
        let cur = tab.get_mut(i).unwrap();
        cur.push(nval);
        cur.insert(0, nval2);
    }
    tab
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

    let tabs: Vec<Vec<Vec<i32>>> = lines.iter().map(|l| process_line(l)).collect();

    let res1: i32 = tabs.iter().map(|t| t[0].last().unwrap()).sum();
    println!("STEP1: {}", res1);

    let res2: i32 = tabs.iter().map(|t| t[0][0]).sum();
    println!("STEP2: {}", res2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process() {
        // step1
        assert_eq!(*process_line("0 3 6 9 12 15")[0].last().unwrap(), 18);
        assert_eq!(*process_line("1 3 6 10 15 21")[0].last().unwrap(), 28);
        assert_eq!(*process_line("10 13 16 21 30 45")[0].last().unwrap(), 68);
        // step2
        assert_eq!(process_line("0 3 6 9 12 15")[0][0], -3);
        assert_eq!(process_line("1 3 6 10 15 21")[0][0], 0);
        assert_eq!(process_line("10 13 16 21 30 45")[0][0], 5);
    }
}
