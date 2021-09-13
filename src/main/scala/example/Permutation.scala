package example

import scala.collection.immutable

/** find all permutations of given list of numbers, each number can only be used once
  * backtracking
  */
trait Permutation {
  object Solution {

    def combine(i: Int, in: List[Int]): Seq[List[Int]] = for {
      m <- 0 to in.size
      (l, r) = in.splitAt(m)
    } yield l ::: List(i) ::: r

    def permute(nums: Array[Int]): List[List[Int]] = {
      def loop(nums: List[Int]): List[List[Int]] =
        nums match {
          case Nil         => Nil
          case head :: Nil => List(List(head))
          case head :: tail =>
            loop(tail).flatMap(combine(head, _))
        }

      loop(nums.toList)
    }
  }

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

  /** recursive way to permutate anything
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

  /** backtracking way of doing it
    */
  def solution3[A](input: Seq[A]): Seq[String] = {
    def loop(
        candidates: Seq[A],
        currentSolution: String,
        solution: Seq[String]
    ): Seq[String] = candidates match {
      case Nil => currentSolution +: solution
      case _ =>
        val p = for {
          c <- candidates
          newCandidates = candidates.filterNot(_ == c)
        } yield {
          loop(newCandidates, currentSolution + c.toString(), solution)
        }
        p.flatten
    }
    loop(input, "", Seq.empty)
  }

  def run(): Unit = {
    // println(solution3(Seq('a', 'b', 'c', 'd')).mkString("\n"))
    // println(solution3((0 to 9).toSeq).mkString("\n"))
    println(Solution.permute((1 to 8).toArray))
    println(Solution.permute(Array(1, 2, 3)))
    println(Solution.permute(Array(0, 1)))
    println(Solution.permute(Array(1)))
  }

}
