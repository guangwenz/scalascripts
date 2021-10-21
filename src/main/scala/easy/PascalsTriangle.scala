package easy

/** Pascal's Triangle
  *
  * Given an integer numRows, return the first numRows of Pascal's triangle.
  *
  * In Pascal's triangle, each number is the sum of the two numbers directly above it as shown:
  *
  * Constraints:
  *
  * 1 <= numRows <= 30
  *
  * https://leetcode.com/problems/pascals-triangle/
  */
trait PascalsTriangle {
  object Solution {
    def generate(numRows: Int): List[List[Int]] = {
      if (numRows == 1) List(List(1))
      else if (numRows == 2) List(List(1), List(1, 1))
      else {
        (3 to numRows).foldLeft(generate(2)) { case (s, n) =>
          val previous = s.last
          s :+ (List(1) ::: (1 until n - 1).map { j =>
            previous(j - 1) + previous(j)
          }.toList ::: List(1))
        }
      }
    }
  }
  def run() = {
    println(Solution.generate(5))
    // println(Solution.generate(1))
  }
}
