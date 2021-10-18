package easy

/** Maximum Subarray
  *
  * Given an integer array nums, find the contiguous subarray (containing at least one number) which has the largest sum and return its sum.
  *
  * A subarray is a contiguous part of an array.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 10^5
  * -10^4 <= nums[i] <= 10^4
  *
  * https://leetcode.com/problems/maximum-subarray/
  */
trait MaximumSubarray {
  object Solution {
    def maxSubArray(nums: Array[Int]): Int = {
      var result = nums.head
      (1 until nums.length).foreach { idx =>
        nums(idx) = math.max(nums(idx), nums(idx) + nums(idx - 1))
        if (nums(idx) > result) result = nums(idx)
      }
      result
    }
  }
  def run() = {
    println(Solution.maxSubArray(Array(-2, 1, -3, 4, -1, 2, 1, -5, 4)) == 6)
    println(Solution.maxSubArray(Array(1)) == 1)
    println(Solution.maxSubArray(Array(5, 4, -1, 7, 8)) == 23)
  }
}
