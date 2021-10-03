package example

/** Spiral Matrix
  * Given an m x n matrix, return all elements of the matrix in spiral order.
  *
  * Constraints:
  *
  * m == matrix.length
  * n == matrix[i].length
  * 1 <= m, n <= 10
  * -100 <= matrix[i][j] <= 100
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/116/array-and-strings/828/
  */
trait SpiralMatrix {

  object Solution {
    val RIGHT = 0
    val DOWN = 1
    val LEFT = 2
    val UP = 3
    def spiralOrder(matrix: Array[Array[Int]]): List[Int] = {
      val Q = collection.mutable.Queue((0, 0))
      val marked = collection.mutable.Map.empty[(Int, Int), Boolean]
      var result = collection.mutable.ListBuffer.empty[Int]
      var direction = RIGHT
      while (Q.nonEmpty) {
        val current = Q.dequeue()
        marked.put(current, true)

        result.addOne(matrix(current._1)(current._2))
        val adj = direction match {
          case RIGHT =>
            if (
              current._2 == matrix.head.length - 1 || marked.contains(
                current._1,
                current._2 + 1
              )
            ) {
              direction = DOWN
              (current._1 + 1, current._2)
            } else (current._1, current._2 + 1)
          case LEFT =>
            if (
              current._2 == 0 || marked.contains(current._1, current._2 - 1)
            ) {
              direction = UP
              (current._1 - 1, current._2)
            } else (current._1, current._2 - 1)

          case DOWN =>
            if (
              current._1 == matrix.length - 1 || marked.contains(
                current._1 + 1,
                current._2
              )
            ) {
              direction = LEFT
              (current._1, current._2 - 1)
            } else (current._1 + 1, current._2)
          case UP =>
            if (
              current._1 == 0 || marked.contains(current._1 - 1, current._2)
            ) {
              direction = RIGHT
              (current._1, current._2 + 1)
            } else (current._1 - 1, current._2)
        }
        if (
          !marked.contains(
            adj
          ) && adj._1 >= 0 && adj._1 < matrix.length && adj._2 >= 0 && adj._2 < matrix.head.length
        ) {
          println(
            s"pushing $adj after current $current with direction $direction"
          )
          Q.enqueue(adj)
        }
      }
      result.toList
    }
  }

  def run() = {
    println(
      Solution
        .spiralOrder(
          Array(Array(1))
        )
        .sameElements(List(1))
    )
    println(
      Solution
        .spiralOrder(
          Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))
        )
        .sameElements(List(1, 2, 3, 6, 9, 8, 7, 4, 5))
    )
    println(
      Solution
        .spiralOrder(
          Array(Array(1, 2, 3, 4), Array(5, 6, 7, 8), Array(9, 10, 11, 12))
        )
        .sameElements(List(1, 2, 3, 4, 8, 12, 11, 10, 9, 5, 6, 7))
    )
  }
}
