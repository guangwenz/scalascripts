package medium

/** Maximum Sum Circular Subarray
  *
  * Given a circular integer array nums of length n, return the maximum possible sum of a non-empty subarray of nums.
  *
  * A circular array means the end of the array connects to the beginning of the array. Formally, the next element of nums[i] is nums[(i + 1) % n] and the previous element of nums[i] is nums[(i - 1 + n) % n].
  *
  * A subarray may only include each element of the fixed buffer nums at most once. Formally, for a subarray nums[i], nums[i + 1], ..., nums[j], there does not exist i <= k1, k2 <= j with k1 % n == k2 % n.
  *
  * Constraints:
  *
  * n == nums.length
  * 1 <= n <= 3 * 10^4
  * -3 * 10^4 <= nums[i] <= 3 * 10^4
  *
  * https://leetcode.com/problems/maximum-sum-circular-subarray/
  */
trait MaximumSumCircularSubarray {
  object Solution {
    def maxSubarraySumCircular(nums: Array[Int]): Int = {
      def maxSubArray(nums: Array[Int]): Int = {
        var result = nums.head
        val dp = Array.fill(nums.length)(0)
        dp(0) = nums.head
        (1 until nums.length).foreach { idx =>
          dp(idx) = math.max(nums(idx), nums(idx) + dp(idx - 1))
          result = math.max(dp(idx), result)
        }
        result
      }
      val maxSub = maxSubArray(nums)
      val reverseMaxSub = {
        val sum = nums.sum
        val invertedMax = maxSubArray(nums.map(i => -i))
        if (sum == -invertedMax) sum else sum + invertedMax
      }

      math.max(maxSub, reverseMaxSub)
    }
  }

  def run() = {
    println(Solution.maxSubarraySumCircular(Array(-2)) == -2)
    println(Solution.maxSubarraySumCircular(Array(1, -2, 3, -2)) == 3)
    println(Solution.maxSubarraySumCircular(Array(5, -3, 5)) == 10)
    println(Solution.maxSubarraySumCircular(Array(3, -1, 2, -1)) == 4)
    println(Solution.maxSubarraySumCircular(Array(3, -2, 2, -3)) == 3)
    println(Solution.maxSubarraySumCircular(Array(-2, -3, -1)) == -1)
    println(
      Solution.maxSubarraySumCircular(Array(-2, 4, -5, 4, -5, 9, 4)) == 15
    )
  }
}
