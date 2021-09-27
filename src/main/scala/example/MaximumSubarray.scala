package example

/** Maximum Subarray
  * Given an integer array nums, find the contiguous subarray (containing at least one number) which has the largest sum and return its sum.
  *
  * A subarray is a contiguous part of an array.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 105
  * -104 <= nums[i] <= 104
  * https://leetcode.com/explore/interview/card/top-interview-questions-easy/97/dynamic-programming/566/
  */
trait MaximumSubarray {
  object Solution {
    def maxSubArray(nums: Array[Int]): Int = {
      // val dp = Array.fill(nums.length)(0)
      // dp(0) = nums.head
      // (1 until nums.length).foreach { i =>
      //   val v = nums(i)
      //   dp(i) = math.max(dp(i - 1) + v, v)
      // }
      // dp.max
      nums.tail
        .foldLeft((nums.head, nums.head)) { case ((lastMax, s), i) =>
          val localM = math.max(s + i, i)
          (if (localM > lastMax) localM else lastMax, localM)
        }
        ._1
    }
  }

  def run() = {
    println(Solution.maxSubArray(Array(-2, 1, -3, 4, -1, 2, 1, -5, 4)) == 6)
    println(Solution.maxSubArray(Array(1)) == 1)
    println(Solution.maxSubArray(Array(5, 4, -1, 7, 8)) == 23)
  }
}
