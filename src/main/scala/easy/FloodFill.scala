package easy

/** Flood Fill
  * An image is represented by an m x n integer grid image where image[i][j] represents the pixel value of the image.
  *
  * You are also given three integers sr, sc, and newColor. You should perform a flood fill on the image starting from the pixel image[sr][sc].
  *
  * To perform a flood fill, consider the starting pixel, plus any pixels connected 4-directionally to the starting pixel of the same color as the starting pixel, plus any pixels connected 4-directionally to those pixels (also with the same color), and so on. Replace the color of all of the aforementioned pixels with newColor.
  *
  * Return the modified image after performing the flood fill.
  * Constraints:
  *
  * m == image.length
  * n == image[i].length
  * 1 <= m, n <= 50
  * 0 <= image[i][j], newColor < 2^16
  * 0 <= sr < m
  * 0 <= sc < n
  *
  * https://leetcode.com/problems/flood-fill/
  */
trait FloodFill {
  object Solution {
    def floodFill(
        image: Array[Array[Int]],
        sr: Int,
        sc: Int,
        newColor: Int
    ): Array[Array[Int]] = {
      val color = image(sr)(sc)
      val Q = collection.mutable.Queue((sr, sc))
      val visited = collection.mutable.Map.empty[(Int, Int), Boolean]
      while (Q.nonEmpty) {
        val (r, c) = Q.dequeue()
        if (image(r)(c) == color) {
          image(r)(c) = newColor
        }
        visited.put((r, c), true)

        for {
          adj <- List((r, c - 1), (r, c + 1), (r - 1, c), (r + 1, c))
          (m, n) = adj
          if (m >= 0 && m < image.size && n >= 0 && n < image.head.length && !visited
            .contains((m, n))) && image(m)(n) == color
        } yield {
          Q.enqueue((m, n))
        }
      }
      println(image.map(_.mkString(",")).mkString("\n"))
      image
    }
  }

  def run() = {
    println(
      Solution.floodFill(
        Array(Array(1, 1, 1), Array(1, 1, 0), Array(1, 0, 1)),
        1,
        1,
        2
      )
    )
    println(Solution.floodFill(Array(Array(0, 0, 0), Array(0, 0, 0)), 0, 0, 2))
  }
}
