package medium

/** Search a 2D Matrix
  *
  * Write an efficient algorithm that searches for a value in an m x n matrix. This matrix has the following properties:
  *
  * Integers in each row are sorted from left to right.
  * The first integer of each row is greater than the last integer of the previous row.
  *
  * Constraints:
  *
  * m == matrix.length
  * n == matrix[i].length
  * 1 <= m, n <= 100
  * -10^4 <= matrix[i][j], target <= 10^4
  *
  * https://leetcode.com/problems/search-a-2d-matrix/
  */
trait Searcha2DMatrix {
  object Solution {
    def searchMatrix(matrix: Array[Array[Int]], target: Int): Boolean = {
      matrix
        .find { r =>
          r.head <= target && r.last >= target
        }
        .exists(_.contains(target))
    }
  }

  def run() = {
    println(
      Solution.searchMatrix(
        Array(Array(1, 3, 5, 7), Array(10, 11, 16, 20), Array(23, 30, 34, 60)),
        3
      ) == true
    )
    println(
      Solution.searchMatrix(
        Array(Array(1, 3, 5, 7), Array(10, 11, 16, 20), Array(23, 30, 34, 60)),
        13
      ) == false
    )
  }
}
