use itertools::Itertools;
use std::cmp::Ordering;
use std::env;
use std::fs;

type Type = u8;
const ONE: Type = 0;
const PAIR: Type = 1;
const TWO_PAIRS: Type = 2;
const THREE: Type = 3;
const FULL_HOUSE: Type = 4;
const FOUR: Type = 5;
const FIVE: Type = 6;

#[derive(PartialEq, Debug)]
struct Hand {
    cards: Vec<u8>,
    bid: u32,
    t: Type,
}

fn int_of_card(c: char) -> u8 {
    match c {
        'A' => 14,
        'K' => 13,
        'Q' => 12,
        'J' => 11,
        'T' => 10,
        c if c.is_ascii_digit() => c.to_digit(10).unwrap() as u8,
        _ => panic!("unknown char: {}", c),
    }
}

fn type_of_hand(cards: &Vec<u8>) -> Type {
    let group_sizes: Vec<usize> = cards
        .iter()
        .sorted()
        .group_by(|e| *e)
        .into_iter()
        .map(|(_, g)| g.count())
        .sorted()
        .rev()
        .collect();
    match group_sizes[0] {
        5 => FIVE,
        4 => FOUR,
        3 if group_sizes[1] == 2 => FULL_HOUSE,
        3 => THREE,
        2 if group_sizes[1] == 2 => TWO_PAIRS,
        2 => PAIR,
        _ => ONE,
    }
}

fn step2_of_step1(hand: &Hand) -> Hand {
    let cards: Vec<u8> = hand
        .cards
        .iter()
        .map(|c| if *c == 11 { 1 } else { *c })
        .collect();

    let (jokers, others): (Vec<u8>, Vec<u8>) = cards.iter().partition(|c| **c == 1);
    let group_sizes: Vec<usize> = others
        .iter()
        .sorted()
        .group_by(|e| *e)
        .into_iter()
        .map(|(_, g)| g.count())
        .sorted()
        .rev()
        .collect();
    let n_jokers = jokers.len();

    let t = if n_jokers == 5 {
        FIVE
    } else {
        match group_sizes[0] + n_jokers {
            5 => FIVE,
            4 => FOUR,
            3 if group_sizes[1] == 2 => FULL_HOUSE,
            3 => THREE,
            2 if group_sizes[1] == 2 => TWO_PAIRS,
            2 => PAIR,
            _ => ONE,
        }
    };

    Hand {
        bid: hand.bid,
        cards,
        t,
    }
}

fn parse_line(line: &str) -> Hand {
    let mut splits = line.split_whitespace();
    let cards: Vec<u8> = splits.next().unwrap().chars().map(int_of_card).collect();
    let bid: u32 = splits.next().unwrap().parse().unwrap();
    let t = type_of_hand(&cards);
    Hand { bid, cards, t }
}

fn compare_hands(a: &Hand, b: &Hand) -> Ordering {
    match a.t.cmp(&b.t) {
        std::cmp::Ordering::Equal => a.cards.iter().cmp(b.cards.iter()),
        o => o,
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

    let mut hands: Vec<Hand> = lines.iter().map(|l| parse_line(l)).collect();
    hands.sort_by(compare_hands);

    let sum = (1u32..)
        .zip(hands.iter())
        .fold(0, |acc, (i, hand)| acc + i * hand.bid);

    println!("STEP1: {}", sum);

    let mut hands: Vec<Hand> = hands.iter().map(step2_of_step1).collect();
    hands.sort_by(compare_hands);
    let sum = (1u32..)
        .zip(hands.iter())
        .fold(0, |acc, (i, hand)| acc + i * hand.bid);

    println!("STEP2: {}", sum);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn h1() -> Hand {
        Hand {
            cards: vec![3, 2, 10, 3, 13],
            bid: 765,
            t: PAIR,
        }
    }
    fn h2() -> Hand {
        Hand {
            cards: vec![10, 5, 5, 11, 5],
            bid: 684,
            t: THREE,
        }
    }
    fn h3() -> Hand {
        Hand {
            cards: vec![12, 12, 12, 11, 14],
            bid: 483,
            t: THREE,
        }
    }

    #[test]
    fn test_parse() {
        assert_eq!(parse_line("32T3K 765"), h1());
        assert_eq!(parse_line("T55J5 684"), h2());
        assert_eq!(parse_line("QQQJA 483"), h3());
    }

    #[test]
    fn test_compare() {
        assert_eq!(compare_hands(&h1(), &h2()), Ordering::Less);
        assert_eq!(compare_hands(&h2(), &h3()), Ordering::Less)
    }

    #[test]
    fn test_step() {
        assert_eq!(step2_of_step1(&h1()), h1());
        assert_eq!(
            step2_of_step1(&h2()),
            Hand {
                bid: 684,
                cards: vec![10, 5, 5, 1, 5],
                t: FOUR
            }
        );
        assert_eq!(
            step2_of_step1(&h3()),
            Hand {
                bid: 483,
                cards: vec![12, 12, 12, 1, 14],
                t: FOUR
            }
        )
    }
}
