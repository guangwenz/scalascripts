package example

/** Set Matrix Zeroes
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/103/array-and-strings/777/
  */
trait SetMatrixZeros {

  /** find all (m,n)s with 0
    * dedupe and set those rows & columns with 0
    */
  object Solution1 {
    def solve(matrix: Array[Array[Int]]): Array[Array[Int]] = {
      val zeroIdx = for {
        (r, m) <- matrix.zipWithIndex
        (c, n) <- r.zipWithIndex
        if c == 0
      } yield (m, n)
      val rows = zeroIdx.map(_._1).distinct
      val cols = zeroIdx.map(_._2).distinct

      rows.foreach(a =>
        (0 until matrix.headOption.map(_.size).getOrElse(0)).foreach(b =>
          matrix(a)(b) = 0
        )
      )
      cols.foreach(c => (0 until matrix.size).foreach(b => matrix(b)(c) = 0))
      matrix
    }
  }

  def showMatrix(in: Array[Array[Int]]): Unit = println(
    in.map(_.mkString(",")).mkString("\n")
  )
  def run() = {
    val in = Array(Array(1, 1, 1), Array(1, 0, 1), Array(1, 1, 1))
    val in2 = Array(Array(0, 1, 2, 0), Array(3, 4, 5, 2), Array(1, 3, 1, 5))
    showMatrix(Solution1.solve(in2))
  }
}
