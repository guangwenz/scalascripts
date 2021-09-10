package example

import java.util.Random
import scala.collection.mutable

/** count the number of islands
  */
trait Island {

  /** not working solution, needs to be fixed
    */
  object Solution1 {
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

    def solve(input: List[List[Int]]): Int = {
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
  }

  /** Solution using DFS graph traversal
    */
  object Solution2 {
    def solve(input: List[List[Int]]): Int = {
      def first(
          visited: mutable.Map[(Int, Int), Boolean]
      ): Option[(Int, Int)] = {
        val p = input.zipWithIndex.collectFirst {
          case (r, m) if r.zipWithIndex.exists { case (c, n) =>
                c == 1 && !visited.contains((m, n))
              } =>
            r.zipWithIndex.collectFirst {
              case (c, n) if c == 1 && !visited.contains((m, n)) => (m, n)
            }
        }
        p.flatten
      }

      def neibours(
          start: (Int, Int),
          visited: mutable.Map[(Int, Int), Boolean]
      ): List[(Int, Int)] = {
        val (x, y) = start
        List(
          (x - 1, y - 1),
          (x - 1, y),
          (x - 1, y + 1),
          (x, y - 1),
          (x, y + 1),
          (x + 1, y - 1),
          (x + 1, y),
          (x + 1, y + 1)
        ).collect {
          case (x, y)
              if x > -1 && x < input.size && y > -1 && y < input.head.size && !visited
                .contains((x, y)) && input(x)(y) == 1 =>
            (x, y)
        }
      }

      //p unvisited point
      def loop(
          p: (Int, Int),
          visited: mutable.Map[(Int, Int), Boolean]
      ): Unit = {
        visited.put(p, true)
        neibours(p, visited) match {
          case Nil => ()
          case head :: tail =>
            (head :: tail).foreach(loop(_, visited))
        }
      }

      val visited = mutable.Map.empty[(Int, Int), Boolean]
      var c = 0
      var next = first(visited)

      while (next.nonEmpty) {
        loop(next.get, visited)
        next = first(visited)
        c = c + 1
      }
      c
    }
  }

  def run(): Unit = {

    // Solution1.gen(20, 20)
    val testCases = Range(1, 11).map { i =>
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
        val ret = Solution2.solve(i)
        if (ret != exp) {
          println(
            s"failed for input \n${i.map(_.mkString(",")).mkString("\n")}, got $ret while expect $exp"
          )
        } else println("Success")
      }
    } yield ()
  }
}
