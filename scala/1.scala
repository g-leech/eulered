//! https://projecteuler.net/problem=1
val PROBLEM = 1

val fizzBuzz = (1 until 1000).filter( _ % 3 == 0 || _ % 5 == 0).sum

assert( fizzBuzz == 233168 ) 
println( "Problem "+ PROBLEM + " solves for " + fizzBuzz )