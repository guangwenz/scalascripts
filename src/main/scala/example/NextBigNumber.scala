package example

import scala.collection.immutable

trait NextBigNumber {

  object Solution1 {

    def swap(in: String, xi: Int, x: Char, yi: Int, y: Char): String = {
      in.zipWithIndex.collect { case (c, i) =>
        if (i == xi) y else if (i == yi) x else c
      }.mkString
    }
    def program(in: String): String = {
      var lastNum = Long.MaxValue
      for ((x, xi) <- in.zipWithIndex) {
        for ((y, yi) <- in.substring(xi).zipWithIndex) {
          val f = swap(in, xi, x, xi + yi, y).toLong
          if (f < lastNum && f > in.toLong) lastNum = f
        }
      }
      if (lastNum == Long.MaxValue) in
      else
        lastNum.toString()
    }
  }

  object Solution2 {
    def toNum(in: List[Int]): Long = in.zipWithIndex.foldLeft(0L) {
      case (s, (n, i)) => s + n * math.pow(10, (in.size - i - 1)).toLong
    }
    def toArr(in: Long): List[Int] =
      in.toString().map(_.toString().toInt).toList

    def program(in: List[Int]): Option[List[Int]] = {
      // def isAccept(c: Int): Boolean = {
      //   println(
      //     s"checking for pos $c in $candidates ${in(c)} < ${in(candidates.head)}"
      //   )
      //   in(c) < in(candidates.head)
      // }
      // //candidate: index before candidate's index
      // //constraint: number < current candidate
      // //goal: next big number
      // def loop(
      //     candidates: List[Int],
      //     solution: List[Long]
      // ): List[Long] = {
      //   if (candidates.isEmpty) solution
      //   else
      //     candidates match {
      //       case head :: next =>
      //         //do we have a solution at head?
      //         if (isAccept(head, candidates)) {
      //           //head accepted, move on to next round of search
      //           loop(next, head.toLong :: solution)
      //         } else loop(next, solution)
      //       case Nil => solution
      //     }
      // }
      // loop((in.size - 1 to 0 by -1).toList, List.empty) match {
      //   case head :: next  => Some(toArr((head :: next).min))
      //   case immutable.Nil => None
      // }
      ???
    }
  }

  def run() {

    val testCases = List(
      // "1" -> "1",
      // "12" -> "21",
      "4312" -> "4321"
      // "1111" -> "1111",
      // "1234" -> "1243",
      // "89875" -> "98875",
      // "23456" -> "23465",
      // "23153" -> "23351"
    )

    for ((i, r) <- testCases) {
      // val c = Solution1.program(i)
      val numL = i.map(_.toString.toInt).toList
      println(numL)
      val c =
        Solution2
          .program(numL)
          .map(x => Solution2.toNum(x).toString)
      if (c.contains(r)) {
        println("Success")
      } else {
        println(s"Fail! result $c with input $i is not equal to expected $r")
      }
    }

  }
}
