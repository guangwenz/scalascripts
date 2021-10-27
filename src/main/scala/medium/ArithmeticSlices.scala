package medium

/** Arithmetic Slices
  * An integer array is called arithmetic if it consists of at least three elements and if the difference between any two consecutive elements is the same.
  *
  * For example, [1,3,5,7,9], [7,7,7,7], and [3,-1,-5,-9] are arithmetic sequences.
  * Given an integer array nums, return the number of arithmetic subarrays of nums.
  *
  * A subarray is a contiguous subsequence of the array.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 5000
  * -1000 <= nums[i] <= 1000
  *
  * https://leetcode.com/problems/arithmetic-slices/
  */
trait ArithmeticSlices {
  object Solution {
    def numberOfArithmeticSlices(nums: Array[Int]): Int = {
      def extend(i: Int, dist: Int): Int = {
        if (i >= nums.length) 0
        else {
          val ret = (i until nums.length)
            .takeWhile(j => nums(j) - nums(j - 1) == dist)
            .size
          ret
        }
      }
      val p = for {
        i <- 0 to nums.length - 3
        dist = nums(i + 1) - nums(i)
        if nums(i + 2) - nums(i + 1) == dist
      } yield 1 + extend(i + 3, dist)
      p.sum
    }
  }

  def run() = {
    println(Solution.numberOfArithmeticSlices(Array(1, 2, 3, 4)) == 3)
    println(Solution.numberOfArithmeticSlices(Array(1, 2, 3, 8, 9, 10)) == 2)
    println(Solution.numberOfArithmeticSlices(Array(1)) == 0)
  }
}
