//! https://projecteuler.net/problem=1
package src.main.scala 

case class Prob1() extends Solution {

	val BOUND = 1000
	val SOLUTION = 233168

	// Gauss' formula for summing arithmetic sequences. Ugly but O(1). 
	// https://nrich.maths.org/2478
	def arithmeticSeries( bound:Int, a:Int, b:Int ): Int = {
	    def youngGaussSum(m:Int) = {
		    val n = bound / m 
		    m * ( n * (n+1) / 2 )
	    }
	    
	    def intersectionComplement( a:Int, b:Int ) = {
	    	youngGaussSum(a) + youngGaussSum(b) - youngGaussSum(ut.lcm(a,b))
	    }

	    (a,b) match {
  			case (a,b) if (a==b) => youngGaussSum(a) - bound
	    	case (a,b) if (bound % a == 0 || bound % b == 0) => {
	    		intersectionComplement(a,b) - bound
	    	}
  			case _ => intersectionComplement(a,b)
  		}
    }
	
	def answer = arithmeticSeries(BOUND, 3, 5)
}

object Test extends App { 
	val problem = Prob1()
	problem.report()
}
