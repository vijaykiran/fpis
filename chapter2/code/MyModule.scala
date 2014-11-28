// A comment!
/* Another Comment */
/** A documentation Comment */
object MyModule {

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }


  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <=0 ) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(pos: Int, cur: Int, next: Int): Int =
      if (pos == n) cur
      else go(pos + 1, next, cur + next)

    go(0, 0, 1)

  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}
