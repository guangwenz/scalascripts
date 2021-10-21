package easy

/** Reshape the Matrix
  * In MATLAB, there is a handy function called reshape which can reshape an m x n matrix into a new one with a different size r x c keeping its original data.
  *
  * You are given an m x n matrix mat and two integers r and c representing the number of rows and the number of columns of the wanted reshaped matrix.
  *
  * The reshaped matrix should be filled with all the elements of the original matrix in the same row-traversing order as they were.
  *
  * If the reshape operation with given parameters is possible and legal, output the new reshaped matrix; Otherwise, output the original matrix.
  *
  * Constraints:
  *
  * m == mat.length
  * n == mat[i].length
  * 1 <= m, n <= 100
  * -1000 <= mat[i][j] <= 1000
  * 1 <= r, c <= 300
  *
  * https://leetcode.com/problems/reshape-the-matrix/
  */
trait ReshapetheMatrix {
  object Solution {
    def matrixReshape(
        mat: Array[Array[Int]],
        r: Int,
        c: Int
    ): Array[Array[Int]] = {
      if (r * c != mat.length * mat.head.length) mat
      else {
        val ret = Array.fill(r, c)(0)
        for {
          ri <- 0 until r
          ci <- 0 until c
        } yield {
          val idx = ri * c + ci
          val mr = idx / mat.head.length
          val mc = idx % mat.head.length
          ret(ri)(ci) = mat(mr)(mc)
        }
        ret
      }
    }
  }

  def run() = {
    println(
      Solution
        .matrixReshape(Array(Array(1, 2), Array(3, 4)), 1, 4)
        .map(_.mkString(","))
        .mkString("\n")
    )
    println(
      Solution
        .matrixReshape(Array(Array(1, 2), Array(3, 4)), 2, 4)
        .map(_.mkString(","))
        .mkString("\n")
    )
    println(
      Solution
        .matrixReshape(Array(Array(1, 2), Array(3, 4)), 4, 1)
        .map(_.mkString(","))
        .mkString("\n")
    )
    println(
      Solution
        .matrixReshape(Array(Array(1, 2, 3, 4)), 2, 2)
        .map(_.mkString(","))
        .mkString("\n")
    )
  }
}
