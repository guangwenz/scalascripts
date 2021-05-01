package example

/** find all permutations of given list of numbers, each number can only be used once
  */
trait Permutation {
  // def _do(input: List[Int], prefix: List[String]): List[String] = input match {
  //   case Nil         => prefix
  //   case head :: Nil => prefix.map(_ + head)
  //   case head :: next =>
  //     for {
  //       i <- Range(0, input.size)
  //       ret <- {
  //       val n = input(i)
  //       val (l, r) = input.splitAt(i)
  //     }
  //     } yield ret
  // }
  // def getNums(i: Int, input: List[Int]): List[Int] = {
  //   val (l, r) = input.splitAt(i)
  //   l ::: r.tail
  // }

  // def _do(input: List[Int], prefix: List[String]): List[String] = {
  //   input match {
  //     case Nil         => prefix
  //     case head :: Nil => prefix.map(_ + head)
  //     case head :: next =>
  //       val p = for {
  //         (i, idx) <- input.zipWithIndex
  //         (l, r) = input.splitAt(idx)
  //         p = prefix.map(n => n + input(idx))
  //       } yield {
  //         _do(l ::: r.tail, p)
  //       }
  //       p.flatten
  //   }
  // }

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
