object Hof {
  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int) : Int =
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)

    loop(0)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int) : Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def findFirst(ss: Array[String], p: String => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int) : Int =
      if (n >= ss.length) -1
      else if (p(ss(n))) n
      else loop(n + 1)

    loop(0)
  }

  //2.2
  // Implement isSorted, which checks whether an Array[A] is sorted according to ordered func
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
    @annotation.tailrec
    def go(i: Int) : Boolean =
      if (as.size - i == 0)  true
      else (as.size
        if (ordered(as(i), as(i + 1))) go(i+1)
        else false

    go(0)
  }



  (x: Int, y: Int) => x < y
  val lessThan = new Function2[Int, Int, Boolean] {
    def apply(a: Int, b: Int) = a < b
  }

  //curry
  def curry[A,B,C](f: (A,B) => C) : A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  //uncurry
  def uncurry[A,B,C](f: A => (B => C)): (A, B) => C =
    (a: A, b: B)  => f(a)(b)

  //compose
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
