//import Prob1

object ERunner extends App {
	if (args.isEmpty)
		println("EXITING: Enter the problem you want an answer to.")
    else {
    	val problem = args(0)
        println(problem)
        //new Prob1().answer()
    }
}