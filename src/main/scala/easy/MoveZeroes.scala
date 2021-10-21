package easy

/** Move Zeroes
  * Given an integer array nums, move all 0's to the end of it while maintaining the relative order of the non-zero elements.
  *
  * Note that you must do this in-place without making a copy of the array.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 10^4
  * -2^31 <= nums[i] <= 2^31 - 1
  *
  * https://leetcode.com/problems/move-zeroes/
  */
trait MoveZeroes {
  object Solution {
    def moveZeroes(nums: Array[Int]): Unit = {
      @annotation.tailrec
      def move(start: Int, end: Int): Unit = {
        if (start < end) {
          if (nums(start) == 0) {
            (start until end).foreach { x =>
              nums(x) = nums(x + 1)
            }
            nums(end) = 0
            move(start, end - 1)
          } else move(start + 1, end)
        }
      }
      move(0, nums.length - 1)
    }
  }

  def run() = {
    val input1 = Array(0, 1, 0, 3, 12)
    Solution.moveZeroes(input1)
    println(input1.sameElements(Array(1, 3, 12, 0, 0)))
    val input2 = Array(0)
    Solution.moveZeroes(input2)
    println(input2.sameElements(Array(0)))
    val input3 = Array(0, 1, 0)
    Solution.moveZeroes(input3)
    println(input3.sameElements(Array(1, 0, 0)))
    val input4 = Array(0, 0, 1)
    Solution.moveZeroes(input4)
    println(input4.sameElements(Array(1, 0, 0)))
  }
}
