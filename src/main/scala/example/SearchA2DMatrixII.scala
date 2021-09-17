package example

/** Search a 2D Matrix II
  *
  * Write an efficient algorithm that searches for a target value in an m x n integer matrix. The matrix has the following properties:
  *
  * Integers in each row are sorted in ascending from left to right.
  * Integers in each column are sorted in ascending from top to bottom.
  *
  * Constraints:
  *
  * m == matrix.length
  * n == matrix[i].length
  * 1 <= n, m <= 300
  * -109 <= matrix[i][j] <= 109
  * All the integers in each row are sorted in ascending order.
  * All the integers in each column are sorted in ascending order.
  * -109 <= target <= 109
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/110/sorting-and-searching/806/
  */

trait SearchA2DMatrixII {
  object Solution {
    def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
      val rowRange: (Int, Int) =
        (
          (0 until matrix.length)
            .map(r => (matrix(r)(matrix.head.size - 1), r))
            .dropWhile(_._1 < target)
            .headOption
            .map(_._2)
            .getOrElse(0),
          matrix.length - 1
        )
      val colRange: (Int, Int) = (
        0,
        (0 until matrix.head.size)
          .map(c => (matrix.head(c), c))
          .takeWhile(_._1 <= target)
          .lastOption
          .map(_._2)
          .getOrElse(matrix.head.size - 1)
      )
      val p = for {
        r <- rowRange._1 to rowRange._2
        c <- colRange._1 to colRange._2
        if (matrix(r)(c) == target)
      } yield (r, c)
      p.nonEmpty
    }
  }

  def run() = {
    println(
      Solution.searchMatrix(
        Array(
          Array(1, 4, 7, 11, 15),
          Array(2, 5, 8, 12, 19),
          Array(3, 6, 9, 16, 22),
          Array(10, 13, 14, 17, 24),
          Array(18, 21, 23, 26, 30)
        ),
        5
      ) == true
    )
    println(
      Solution.searchMatrix(
        Array(
          Array(1, 4, 7, 11, 15),
          Array(2, 5, 8, 12, 19),
          Array(3, 6, 9, 16, 22),
          Array(10, 13, 14, 17, 24),
          Array(18, 21, 23, 26, 30)
        ),
        20
      ) == false
    )
    println(
      Solution.searchMatrix(
        Array(
          Array(-1, 3)
        ),
        3
      ) == true
    )
  }
}
