package example

/** Kth Smallest Element in a Sorted Matrix
  *
  * Given an n x n matrix where each of the rows and columns are sorted in ascending order, return the kth smallest element in the matrix.
  *
  * Note that it is the kth smallest element in the sorted order, not the kth distinct element.
  *
  * Constraints:
  *
  * n == matrix.length
  * n == matrix[i].length
  * 1 <= n <= 300
  * -10^9 <= matrix[i][j] <= 10^9
  * All the rows and columns of matrix are guaranteed to be sorted in non-decreasing order.
  * 1 <= k <= n^2
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/120/sorting-and-searching/858/
  */
trait KthSmallestElementinaSortedMatrix {
  object Solution {
    def kthSmallest(matrix: Array[Array[Int]], k: Int): Int = {
      @annotation.tailrec
      def merge(
          source: Vector[Int],
          target: Array[Int],
          acc: Vector[Int]
      ): Vector[Int] = {
        if (source.isEmpty) acc ++ target
        else if (target.isEmpty) acc ++ source
        else {
          if (source.head < target.head)
            merge(source.tail, target, acc :+ source.head)
          else merge(source, target.tail, acc :+ target.head)
        }
      }
      val merged = matrix.foldLeft(Vector.empty[Int])(merge(_, _, Vector.empty))

      merged(k - 1)
    }
  }

  def run() = {
    println(Solution.kthSmallest(Array(Array(-5)), 1) == -5)
    println(
      Solution.kthSmallest(
        Array(Array(1, 5, 9), Array(10, 11, 13), Array(12, 13, 15)),
        8
      ) == 13
    )
  }
}
