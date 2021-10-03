package example

/** First Missing Positive
  * Given an unsorted integer array nums, return the smallest missing positive integer.
  *
  * You must implement an algorithm that runs in O(n) time and uses constant extra space.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 5 * 10^5
  * -2^31 <= nums[i] <= 2^31 - 1
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/116/array-and-strings/832/
  */
trait FirstMissingPositive {
  object Solution {
    def firstMissingPositive(nums: Array[Int]): Int = {
      val sorted = nums.sorted
      if (sorted.head > 1) 1
      else {
        val positives = sorted.dropWhile(_ <= 0).distinct
        for {
          m <- (1 to positives.length)
        } yield {
          if (positives(m - 1) != m) return m
        }
        if (positives.isEmpty) 1 else positives.last + 1
      }
    }
  }

  def run() = {
    println(Solution.firstMissingPositive(Array(1, 2, 0)) == 3)
    println(Solution.firstMissingPositive(Array(3, 4, -1, 1)) == 2)
    println(Solution.firstMissingPositive(Array(7, 8, 9, 11, 12)) == 1)
    println(Solution.firstMissingPositive(Array(0)) == 1)
    println(Solution.firstMissingPositive(Array(0, 2, 2, 1, 1)) == 3)
  }
}
