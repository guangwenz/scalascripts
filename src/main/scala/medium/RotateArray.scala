package medium

/** Rotate Array
  *
  * Given an array, rotate the array to the right by k steps, where k is non-negative.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 10^5
  * -2^31 <= nums[i] <= 2^31 - 1
  * 0 <= k <= 10^5
  *
  * https://leetcode.com/problems/rotate-array/
  *
  * Follow up:
  *
  * Try to come up with as many solutions as you can. There are at least three different ways to solve this problem.
  * Could you do it in-place with O(1) extra space?
  */
trait RotateArray {
  object Solution {
    def rotate(nums: Array[Int], k: Int): Unit = {
      val full = k % nums.length
      val updated = nums.takeRight(full) ++ nums.dropRight(full)
      nums.indices.foreach { i =>
        nums(i) = updated(i)
      }
    }
  }

  def run() = {
    val input = Array(1, 2, 3, 4, 5, 6, 7)
    Solution.rotate(input, 3)
    println(input.sameElements(Array(5, 6, 7, 1, 2, 3, 4)))
    val input2 = Array(-1, -100, 3, 99)
    Solution.rotate(input2, 2)
    println(input2.sameElements(Array(3, 99, -1, -100)))
  }
}
