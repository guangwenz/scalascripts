package example

/** Longest Increasing Path in a Matrix
  *
  * Given an m x n integers matrix, return the length of the longest increasing path in matrix.
  *
  * From each cell, you can either move in four directions: left, right, up, or down. You may not move diagonally or move outside the boundary (i.e., wrap-around is not allowed).
  *
  * Constraints:
  *
  * m == matrix.length
  * n == matrix[i].length
  * 1 <= m, n <= 200
  * 0 <= matrix[i][j] <= 2^31 - 1
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/118/trees-and-graphs/849/
  */
trait LongestIncreasingPathinMatrix {
  object Solution {
    case class Node(position: (Int, Int), value: Int)
    def longestIncreasingPath(matrix: Array[Array[Int]]): Int = {
      val memo = collection.mutable.Map.empty[Node, Int]
      def buildGraph: Map[Node, List[Node]] = {
        val g = collection.mutable.Map.empty[Node, List[Node]]
        for {
          r <- matrix.indices
          c <- matrix.head.indices
          neibours = List((r, c - 1), (r, c + 1), (r - 1, c), (r + 1, c))
            .collect {
              case (r, c)
                  if r >= 0 && r < matrix.length && c >= 0 && c < matrix.head.length =>
                Node((r, c), matrix(r)(c))
            }
          _ = {
            g.put(Node((r, c), matrix(r)(c)), neibours)
          }
        } yield ()
        g.toMap
      }

      def longestFrom(start: Node, g: Map[Node, List[Node]]): Int = {
        if (memo.contains(start)) memo(start)
        else {
          val adj =
            g(start)
              .filter(_.value > start.value)

          val ret =
            if (adj.isEmpty) 1
            else {
              adj.map(n => longestFrom(n, g) + 1).max
            }
          memo.put(start, ret)
          ret
        }
      }

      val g = buildGraph
      var ret = 0
      for {
        (v, adj) <- g
        _ = {
          val depth = longestFrom(v, g)
          // println(s"long depth from $v(adj: $adj) is $depth")
          if (depth > ret) ret = depth
        }
      } yield ()
      ret
    }
  }

  def run() = {
    println(Solution.longestIncreasingPath(Array(Array(1))) == 1)
    println(
      Solution.longestIncreasingPath(
        Array(Array(3, 4, 5), Array(3, 2, 6), Array(2, 2, 1))
      ) == 4
    )
    println(
      Solution.longestIncreasingPath(
        Array(Array(9, 9, 4), Array(6, 6, 8), Array(2, 1, 1))
      ) == 4
    )
  }
}
