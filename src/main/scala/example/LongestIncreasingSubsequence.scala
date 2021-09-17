package example

/** Given an integer array nums, return the length of the longest strictly increasing subsequence.
  * A subsequence is a sequence that can be derived from an array by deleting some or no elements without changing the order of the remaining elements. For example, [3,6,2,7] is a subsequence of the array [0,3,1,6,2,2,7].
  *
  * Constraints:
  *
  *   1 <= nums.length <= 2500
  *   -104 <= nums[i] <= 104
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/111/dynamic-programming/810/
  */
trait LongestIncreasingSubsequence {

  /** dp table solution
    */
  object Solution {
    def lengthOfLIS(nums: Array[Int]): Int = {
      val dp = Array.fill(nums.size)(1)
      for {
        m <- 1 until nums.size
      } yield {
        val p = for {
          n <- (0 until m)
          if nums(n) < nums(m)
        } yield dp(n) + 1
        if (p.nonEmpty)
          dp(m) = p.max
      }
      dp.max
    }
  }

  def run(): Unit = {
    val testCases = Map(
      List(3, 10, 2, 1, 20) -> 3,
      List(2, 7, 4, 3, 8) -> 3,
      List(2, 4, 3, 7, 4, 5) -> 4,
      List(3, 2) -> 1,
      List(50, 3, 10, 7, 40, 80) -> 4,
      List(3, 10, 7) -> 2,
      List(4, 10, 4, 3, 8, 9) -> 3
    )
    for {
      (i, exp) <- testCases
      ret = Solution.lengthOfLIS(i.toArray)
      _ = {
        if (ret != exp)
          println(s"Failed for input $i, expected $exp but got $ret")
        else println("Success")
      }
    } yield ()
  }
}
