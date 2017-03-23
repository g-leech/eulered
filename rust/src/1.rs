//! https://projecteuler.net/problem=1
#![warn(bad_style, unused)]

mod utils;

const  PROBLEM: u16 = 1;
const  BOUND: u32 = 1000;


fn fizzes(i: u32) -> bool {
	i % 3 == 0
}

fn buzzes(i: u32) -> bool {
	i % 5 == 0
}

fn fizz_buzz(bound: u32) -> u32 {
    (1..bound)
    	.filter( |&i| fizzes(i) || buzzes(i) )
    	.fold(0, |sum, i| sum + i)
}

fn solution(bound: u32) -> String {
    fizz_buzz(bound).to_string()
}


fn main() {
	let soln = solution(BOUND);
	assert_eq!("233168", soln);
	println!("Problem \"{}\" solves for {}", PROBLEM, soln);
}
