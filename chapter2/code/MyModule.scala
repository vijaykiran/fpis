// A comment!
/* Another Comment */
/** A documentation Comment */
object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  // private def formatAbs(x: Int) = {
  //   val msg = "The absolute value of %d is %d"
  //   msg.format(x, abs(x))
  // }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <=0 ) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  // private def formatFactorial(n: Int)  = {
  //   val msg = "The factorial of %d is %d."
  //   msg.format(n, factorial(n))
  // }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(pos: Int, cur: Int, next: Int): Int =
      if (pos == n) cur
      else go(pos + 1, next, cur + next)

    go(0, 0, 1)

  }

  def fib2(n: Int) = if (n <=1) n else fib2(n-1) + fib2(n-2)

  //Take function as argument
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("abs", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("nth fib at location", 10, fib))
  }

  //Above Results in:
  /*
   The abs of -42 is 42
   The factorial of 7 is 5040
   The nth fib at location of 10 is 55
   */

}
