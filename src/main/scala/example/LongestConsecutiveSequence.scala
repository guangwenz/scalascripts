package example

/** Longest Consecutive Sequence
  * Given an unsorted array of integers nums, return the length of the longest consecutive elements sequence.
  *
  * You must write an algorithm that runs in O(n) time.
  *
  * Constraints:
  *
  * 0 <= nums.length <= 10^5
  * -10^9 <= nums[i] <= 10^9
  */
trait LongestConsecutiveSequence {
  object Solution {
    def longestConsecutive(nums: Array[Int]): Int = {
      val sorted = nums.sorted.distinct
      val dp = Array.fill(sorted.length)(1)
      (1 until sorted.length).foreach { i =>
        if (sorted(i) - sorted(i - 1) == 1) dp(i) = dp(i - 1) + 1
      }
      if (dp.isEmpty) 0 else dp.max
    }
  }

  def run() = {
    println(Solution.longestConsecutive(Array()) == 0)
    println(Solution.longestConsecutive(Array(1, 2, 0, 1)) == 3)
    println(Solution.longestConsecutive(Array(100, 4, 200, 1, 3, 2)) == 4)
    println(
      Solution.longestConsecutive(Array(0, 3, 7, 2, 5, 8, 4, 6, 0, 1)) == 9
    )
  }
}
