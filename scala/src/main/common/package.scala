package eulered.common

import java.io.File


object common {

  // What a hack! Get problem from filename of a spurious exception.
  def getProblemNum = new Exception().getStackTrace.head.getFileName.replace(".scala", "")

  def lcm( a: Int, b: Int ) = (a * b) / gcd(a, b)
  
  def gcd( a: Int, b: Int ): Int = if (b==0) a else gcd( b, a % b )
  

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }


  def subFile(file: File, children: String*) = children.foldLeft(file)((file, child) => new File(file, child))

  
}
