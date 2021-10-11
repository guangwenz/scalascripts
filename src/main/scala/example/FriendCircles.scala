package example

/** Friend Circles
  * There are n cities. Some of them are connected, while some are not. If city a is connected directly with city b, and city b is connected directly with city c, then city a is connected indirectly with city c.
  *
  * A province is a group of directly or indirectly connected cities and no other cities outside of the group.
  *
  * You are given an n x n matrix isConnected where isConnected[i][j] = 1 if the ith city and the jth city are directly connected, and isConnected[i][j] = 0 otherwise.
  *
  * Return the total number of provinces.
  *
  * Constraints:
  *
  * 1 <= n <= 200
  * n == isConnected.length
  * n == isConnected[i].length
  * isConnected[i][j] is 1 or 0.
  * isConnected[i][i] == 1
  * isConnected[i][j] == isConnected[j][i]
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/118/trees-and-graphs/846/
  */
trait FriendCircles {
  object Solution {
    import collection.mutable.{Map => MMap}
    def findCircleNum(isConnected: Array[Array[Int]]): Int = {
      val visited = MMap.empty[(Int, Int), Boolean]
      def visit(start: (Int, Int)): Unit = {
        val Q = collection.mutable.Stack(start)
        while (Q.nonEmpty) {
          val current = Q.pop()
          visited.put(current, true)
          val (row, col) = current
          for {
            adj <- List(
              (row - 1, col),
              (row + 1, col),
              (row, col - 1),
              (row, col + 1)
            )
            if !visited.contains(adj) && {
              val (r, c) = adj
              r >= 0 && r < isConnected.length && c >= 0 && c < isConnected.head.length && isConnected(
                r
              )(c) == 1
            }
          } yield {
            Q.push(adj)
          }
        }
      }
      val program = for {
        row <- Array.range(0, isConnected.length)
        col <- Array.range(0, isConnected.head.length)
        if (isConnected(row)(col) == 1 && !visited.contains((row, col)))
        _ = visit((row, col))
      } yield 1
      program.sum
    }
  }

  def run() = {
    println(
      Solution.findCircleNum(
        Array(Array(1, 1, 0), Array(1, 1, 0), Array(0, 0, 1))
      ) == 2
    )
    println(
      Solution.findCircleNum(
        Array(Array(1, 0, 0), Array(0, 1, 0), Array(0, 0, 1))
      ) == 3
    )
  }
}
