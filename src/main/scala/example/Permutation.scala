package example

import scala.collection.immutable

/** find all permutations of given list of numbers, each number can only be used once
  * backtracking
  */
trait Permutation {

  /** brutal force
    */
  def solution1(input: Seq[Int]): Seq[String] =
    for {
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

  /** generalized solution to permutate anything
    */
  def solution2[A](input: Seq[A]): Seq[String] = {
    val f = for {
      (i, x) <- input.zipWithIndex
      (head, tail) = input.splitAt(x)

    } yield {
      solution2(head ++ tail.tail) match {
        case Nil    => Seq(i.toString())
        case nonEmp => nonEmp.map(i.toString() ++ _)
      }
    }
    f.flatten
  }

  def run(): Unit = {
    println(solution2(Seq('a', 'b', 'c', 'd')).mkString("\n"))
  }

}
