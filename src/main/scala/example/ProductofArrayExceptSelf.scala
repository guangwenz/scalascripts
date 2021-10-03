package example

/** Product of Array Except Self
  * Given an integer array nums, return an array answer such that answer[i] is equal to the product of all the elements of nums except nums[i].
  *
  * The product of any prefix or suffix of nums is guaranteed to fit in a 32-bit integer.
  *
  * You must write an algorithm that runs in O(n) time and without using the division operation.
  *
  * Constraints:
  *
  * 2 <= nums.length <= 10^5
  * -30 <= nums[i] <= 30
  * The product of any prefix or suffix of nums is guaranteed to fit in a 32-bit integer.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/116/array-and-strings/827/
  */
trait ProductofArrayExceptSelf {
  object Solution {
    def productExceptSelf(nums: Array[Int]): Array[Int] = {
      val left = Array.fill(nums.length)(1)
      val right = Array.fill(nums.length)(1)
      left(0) = nums.head
      right(0) = nums(nums.length - 1)
      for {
        i <- 1 until nums.length
        lv = nums(i)
        rv = nums(nums.length - i - 1)
      } yield {
        left(i) = left(i - 1) * lv
        right(i) = right(i - 1) * rv
      }
      nums(0) = 1 * right(nums.length - 2)
      (1 until nums.length).foreach { i =>
        val (l, r) = (i - 1, nums.length - i - 2)
        val a = if (l < 0 || l > left.length - 1) 1 else left(l)
        val b = if (r < 0 || r > right.length - 1) 1 else right(r)
        nums(i) = a * b
      }
      nums
    }
  }

  def run() = {
    println(
      Solution
        .productExceptSelf(Array(1, 2, 3, 4))
        .sameElements(Array(24, 12, 8, 6))
    )
    println(
      Solution
        .productExceptSelf(Array(-1, 1, 0, -3, 3))
        .sameElements(Array(0, 0, 9, 0, 0))
    )
    println(
      Solution
        .productExceptSelf(Array(4, 3, 2, 1, 2))
        .sameElements(Array(12, 16, 24, 48, 24))
    )
  }
}
