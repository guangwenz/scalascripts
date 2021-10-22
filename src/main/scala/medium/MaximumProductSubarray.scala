package medium

/** Maximum Product Subarray
  *
  * Given an integer array nums, find a contiguous non-empty subarray within the array that has the largest product, and return the product.
  *
  * It is guaranteed that the answer will fit in a 32-bit integer.
  *
  * A subarray is a contiguous subsequence of the array.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 2 * 10^4
  * -10 <= nums[i] <= 10
  * The product of any prefix or suffix of nums is guaranteed to fit in a 32-bit integer.
  *
  * https://leetcode.com/problems/maximum-product-subarray/
  */
trait MaximumProductSubarray {
  object Solution {
    def maxProduct(nums: Array[Int]): Int = {
      def maxProduct(start: Int): Int = {
        var result = nums(start)
        (start until nums.length).foldLeft(1) { case (s, i) =>
          val ret = s * nums(i)
          result = math.max(result, ret)
          ret
        }
        result
      }
      var result = nums.head
      for {
        i <- nums.indices
        p = maxProduct(i)
      } yield {
        result = math.max(result, p)
      }
      result
    }
  }
  def run() = {
    println(Solution.maxProduct(Array(2, 3, -2, 4)) == 6)
    println(Solution.maxProduct(Array(-2, 0, -1)) == 0)
    println(Solution.maxProduct(Array(-2, 3, -4)) == 24)
    println(Solution.maxProduct(Array(-2)) == -2)
  }
}
