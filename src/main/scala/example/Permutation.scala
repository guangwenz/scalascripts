package example

/** find all permutations of given list of numbers, each number can only be used once
  */
trait Permutation {

  def run(): Unit = {
    val input =
      List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) //List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val ret = for {
      a <- input if a != 0
      b <- input if b != a
      c <- input if c != a && c != b
      d <- input if d != c && d != b && d != a
      e <- input if e != d && e != c && e != b && e != a
      f <- input if f != e && f != d && f != c && f != b && f != a
      g <- input if g != f && g != e && g != d && g != c && g != b && g != a
      h <- input
      if h != g && h != f && h != e && h != d && h != c && h != b && h != a
    } yield "" + a + b + c + d + e + f + g + h

    println(ret.mkString("\n"))
  }
}
