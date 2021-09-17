package example

/** Unique Paths
  * A robot is located at the top-left corner of a m x n grid (marked 'Start' in the diagram below).
  * Constraints:
  *  1 <= m, n <= 100
  *  It's guaranteed that the answer will be less than or equal to 2 * 109.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/111/dynamic-programming/808/
  */
trait UniquePaths {

  /** DP table
    */
  object Solution {
    def uniquePaths(m: Int, n: Int): Int = {
      val dp = Array.fill(m, n)(1)
      for {
        r <- 1 until m
        c <- 1 until n
      } yield {
        dp(r)(c) = dp(r)(c - 1) + dp(r - 1)(c)
      }
      dp(m - 1)(n - 1)
    }
  }

  /** recursive
    * mem exceeded exception
    */
  object Solution1 {
    // @annotation.tailrec
    def uniquePaths(m: Int, n: Int): Int = {
      if (m == 1 || n == 1) 1
      else uniquePaths(m - 1, n) + uniquePaths(m, n - 1)
    }
  }

  def run() = {
    println(Solution.uniquePaths(3, 7) == 28)
    println(Solution.uniquePaths(3, 2) == 3)
    println(Solution.uniquePaths(7, 3) == 28)
    println(Solution.uniquePaths(3, 3) == 6)
    println(Solution.uniquePaths(2, 2) == 2)
    println(Solution.uniquePaths(13, 23))
  }
}
