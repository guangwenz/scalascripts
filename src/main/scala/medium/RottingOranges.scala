package medium

/** Rotting Oranges
  *
  * You are given an m x n grid where each cell can have one of three values:
  *
  * 0 representing an empty cell,
  * 1 representing a fresh orange, or
  * 2 representing a rotten orange.
  * Every minute, any fresh orange that is 4-directionally adjacent to a rotten orange becomes rotten.
  *
  * Return the minimum number of minutes that must elapse until no cell has a fresh orange. If this is impossible, return -1.
  *
  * Constraints:
  *
  * m == grid.length
  * n == grid[i].length
  * 1 <= m, n <= 10
  * grid[i][j] is 0, 1, or 2.
  *
  * https://leetcode.com/problems/rotting-oranges/
  */
trait RottingOranges {
  object Solution {
    def orangesRotting(grid: Array[Array[Int]]): Int = {
      val visitedFreshMap = collection.mutable.Map.empty[(Int, Int), Int]

      def bfs(r: Int, c: Int): Int = {
        var result = -1
        val Q = collection.mutable.Queue(((r, c), 1))
        val visited = collection.mutable.Map.empty[(Int, Int), true]
        while (Q.nonEmpty) {
          val ((m, n), level) = Q.dequeue()
          visited.put((m, n), true)
          for {
            adj <- List((m + 1, n), (m - 1, n), (m, n - 1), (m, n + 1))
            (x, y) = adj
            if (x >= 0 && y >= 0 && x < grid.size && y < grid.head.size) && !visited
              .contains((x, y)) && (grid(x)(y) == 1 || grid(x)(y) == 2)
          } yield {
            if (grid(x)(y) == 2) return level
            else Q.enqueue(((x, y), level + 1))
          }
        }
        return -1
      }

      var result = 0
      for {
        i <- grid.indices
        j <- grid.head.indices
        if grid(i)(j) == 1
        minimum = bfs(i, j)
      } yield {
        if (minimum == -1) return -1
        else {
          visitedFreshMap.put((i, j), minimum)
          result = math.max(result, minimum)
        }
      }
      result
    }
  }

  def run() = {
    println(
      Solution.orangesRotting(
        Array(Array(2, 1, 1), Array(1, 1, 0), Array(0, 1, 1))
      ) == 4
    )
    println(
      Solution.orangesRotting(
        Array(Array(2, 1, 1), Array(0, 1, 1), Array(1, 0, 1))
      ) == -1
    )
    println(Solution.orangesRotting(Array(Array(0, 2))) == 0)
    println(Solution.orangesRotting(Array(Array(0, 1))) == -1)
    println(
      Solution.orangesRotting(
        Array(Array(2, 1, 1), Array(1, 1, 1), Array(0, 1, 2))
      ) == 2
    )
  }

}
