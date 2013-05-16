package ae.mohd874.algorithms.numbers

object Fibonacci {

  def apply(n: Int): BigInt = {
    fibonacci(n) drop n head
  }
  
  /*
   * Lesson Learned: 
   * KISS: Keep It Simple Stupid OR Keep It Short & Simple
   * 
   * I was trying to create a function that can return an nth fibonacci number
   * even if the request is crazy (e.g. the 1000000000th fibonacci number). My function was the following:
   *
   *  def fibonacci(nth: BigInt): BigInt = {
	    def nthFibonacci(f1: BigInt, f2: BigInt, n: BigInt): BigInt = {
	      n match {
	        case zero if(zero == 0) => 0 
	      	case x if (x >= nth) => f2
	        case x => {
	          nthFibonacci(f2, f1 + f2, x + 1)
	        }
	      }
	    }
	    nthFibonacci(0, 1, 0)
	  }
   *
   * My idea was to create a tail recursive function that uses the f2 aurgument as an accmulator. 
   * However, the function worked fine except for the first 2 or three numbers. 
   * I was pissed as I worked on it for few hours. I decisded to look for examples
   * to see how can I fix it. 
   * 
   * I found on StackOverflow a question about the best way to create fibonacci number. 
   * The answer was like a shockwave that split my mind. 
   * 
   * So simple and elegent yet very effecint. I could not work on my version anymore.
   * 
   * Here I pay my full respect "Jed Wesley-Smith" for the wonderful solution which 
   * I used below.  
   * 
   * I hope this lesson will be engraved in my mind forever.
   * 
   * Source: http://stackoverflow.com/questions/7388416/what-is-the-fastest-way-to-write-fibonacci-function-in-scala
   */
  
  def fibonacci(n: Int): Stream[BigInt] = {
    def tail(h: BigInt, n: BigInt): Stream[BigInt] = h #:: tail(n, h + n)
    tail(0, 1)
  }
  
  def asString(n: Int): String = {
    apply(n) toString
  }
}