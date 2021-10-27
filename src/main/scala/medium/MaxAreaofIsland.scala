package medium

/** Max Area of Island
  * You are given an m x n binary matrix grid. An island is a group of 1's (representing land) connected 4-directionally (horizontal or vertical.) You may assume all four edges of the grid are surrounded by water.
  *
  * The area of an island is the number of cells with a value 1 in the island.
  *
  * Return the maximum area of an island in grid. If there is no island, return 0.
  *
  * Constraints:
  *
  * m == grid.length
  * n == grid[i].length
  * 1 <= m, n <= 50
  * grid[i][j] is either 0 or 1.
  *
  * https://leetcode.com/problems/max-area-of-island/
  */
trait MaxAreaofIsland {
  object Solution {
    def maxAreaOfIsland(grid: Array[Array[Int]]): Int = {
      val visited = collection.mutable.Map.empty[(Int, Int), Boolean]
      var result = 0

      def islandArea(sr: Int, sc: Int): Int = {
        val Q = collection.mutable.Queue((sr, sc))
        visited.put((sr, sc), true)
        var result = 1
        while (Q.nonEmpty) {
          val (r, c) = Q.dequeue()
          for {
            adj <- List((r, c - 1), (r, c + 1), (r - 1, c), (r + 1, c))
            (m, n) = adj
            if (m >= 0 && m < grid.length && n >= 0 && n < grid.head.length && !visited
              .contains((m, n))) && grid(m)(n) == 1
          } yield {
            result += 1
            visited.put((m, n), true)
            Q.enqueue((m, n))
          }
        }
        result
      }
      for {
        i <- 0 until grid.length
        j <- 0 until grid.head.length
        if grid(i)(j) == 1 && !visited.contains((i, j))
        area = islandArea(i, j)
      } yield {
        result = math.max(area, result)
      }
      result
    }
  }
  def run() = {
    println(
      Solution.maxAreaOfIsland(
        Array(
          Array(0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
          Array(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0),
          Array(0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
          Array(0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0),
          Array(0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0),
          Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
          Array(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0),
          Array(0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0)
        )
      ) == 6
    )
    println(Solution.maxAreaOfIsland(Array(Array(0, 0, 0, 0, 0, 0, 0, 0))) == 0)
  }
}
