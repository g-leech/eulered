package src.main.scala 

abstract class Solution {

	val PROJECT = "src.main.scala.Prob"
	val PROBLEM = getProblemNum
	val SOLUTION: Any

  	def answer: Any

  	def report() = {
    	val result = answer

    	assert( result == SOLUTION )
		println( "Problem "+ PROBLEM + " solves for " + result )
	}

	lazy val getProblemNum = getClass.getName.replace(PROJECT, "")

}