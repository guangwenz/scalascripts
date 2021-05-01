package example

import java.util.Random
import scala.collection.mutable

/** count the number of islands
  */
trait Island {
  def gen(rNum: Int, cNum: Int): Unit = {
    val ret = for {
      r <- Range(1, rNum)
      c = Range(1, cNum).foldLeft("") { case (s, _) =>
        val f = scala.util.Random.nextBoolean()
        val n = if (f) 1 else 0
        if (s.isEmpty) n.toString else s + " " + n
      }
    } yield c
    println(ret.mkString("\n"))
  }

  def loop(
      start: (Int, Int),
      input: List[List[Int]],
      visited: mutable.Map[(Int, Int), Boolean]
  ): Unit = {
    val (r, c) = start
    visited.put(start, true)
    val round =
      List((r, c + 1), (r + 1, c - 1), (r + 1, c), (r + 1, c + 1))
        .collect {
          case (rr, rc)
              if !visited.contains(
                (rr, rc)
              ) && rr > -1 && rc > -1 && rr < input.size && rc < input.head.size && input(
                rr
              )(
                rc
              ) == 1 =>
            (rr, rc)
        }
    round.foreach(loop(_, input, visited))
  }

  def solution(input: List[List[Int]]): Int = {
    val visited = mutable.Map.empty[(Int, Int), Boolean]
    val rounds = for {
      (r, rIdx) <- input.zipWithIndex
      (c, cIdx) <- r.zipWithIndex if c == 1 && !visited.contains((rIdx, cIdx))
      ret = {
        loop((rIdx, cIdx), input, visited)
        1
      }
    } yield ret
    rounds.sum
  }

  def run(): Unit = {
    val testCases = Range(1, 7).map { i =>
      val uri = s"island/$i.txt"
      val inputs = scala.io.Source
        .fromResource(uri)
        .getLines()
        .toList

      (
        inputs.tail.tail.map(_.split(" ").map(_.toInt).toList),
        inputs.head.toInt
      )
    }

    for {
      (i, exp) <- testCases
      _ = {
        val ret = solution(i)
        if (ret != exp) {
          println(
            s"failed for input \n${i.map(_.mkString(",")).mkString("\n")}, got $ret while expect $exp"
          )
        }
      }
    } yield ()
  }
}
