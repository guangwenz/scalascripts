package medium

/** 01 Matrix
  * Given an m x n binary matrix mat, return the distance of the nearest 0 for each cell.
  *
  * The distance between two adjacent cells is 1.
  *
  * Constraints:
  *
  * m == mat.length
  * n == mat[i].length
  * 1 <= m, n <= 10^4
  * 1 <= m * n <= 10^4
  * mat[i][j] is either 0 or 1.
  * There is at least one 0 in mat.
  *
  * https://leetcode.com/problems/01-matrix/
  */
trait `01Matrix` {
  object Solution {
    def updateMatrix(mat: Array[Array[Int]]): Array[Array[Int]] = {
      val ret = Array.fill(mat.length, mat.head.length)(Int.MaxValue)
      val Q = collection.mutable.Queue.empty[(Int, Int)]
      for {
        i <- mat.indices
        j <- mat.head.indices
        if mat(i)(j) == 0
      } yield {
        ret(i)(j) = 0
        Q.enqueue((i, j))
      }
      while (Q.nonEmpty) {
        val (r, c) = Q.dequeue()
        for {
          (m, n) <- List((r, c - 1), (r, c + 1), (r - 1, c), (r + 1, c))
          if m >= 0 && m < mat.length && n >= 0 && n < mat.head.length
        } yield {
          if (ret(m)(n) > ret(r)(c) + 1) {
            ret(m)(n) = ret(r)(c) + 1
            Q.enqueue((m, n))
          }
        }
      }
      ret
    }
  }

  def show(mat: Array[Array[Int]]): Unit = {
    println(mat.map(_.mkString(",")).mkString("\n"))
  }
  def run() = {
    show(
      Solution.updateMatrix(
        Array(Array(0, 0, 0), Array(0, 1, 0), Array(0, 0, 0))
      )
    )
    show(
      Solution.updateMatrix(
        Array(Array(0, 0, 0), Array(0, 1, 0), Array(1, 1, 1))
      )
    )
    show(
      Solution.updateMatrix(
        Array(Array(0), Array(1))
      )
    )
    val before = Array(
      Array(0, 1, 0, 1, 1),
      Array(1, 1, 0, 0, 1),
      Array(0, 0, 0, 1, 0),
      Array(1, 0, 1, 1, 1),
      Array(1, 0, 0, 0, 1)
    )
    show(
      Solution.updateMatrix(
        before
      )
    )
    show(
      Solution.updateMatrix(Array(Array.fill(9999)(1) ++ Array(0)))
    )
  }
}
