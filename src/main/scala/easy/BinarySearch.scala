package easy

/** Binary Search
  * Given an array of integers nums which is sorted in ascending order, and an integer target, write a function to search target in nums. If target exists, then return its index. Otherwise, return -1.
  *
  * You must write an algorithm with O(log n) runtime complexity.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 10^4
  * -10^4 < nums[i], target < 10^4
  * All the integers in nums are unique.
  * nums is sorted in ascending order.
  *
  * https://leetcode.com/problems/binary-search/
  */
trait BinarySearch {
  object Solution {
    def search(nums: Array[Int], target: Int): Int = {
      @annotation.tailrec
      def inner(start: Int, end: Int): Int = {
        if (start > end) -1
        else {
          val m = (start + end) / 2
          nums(m) - target match {
            case 0          => m
            case x if x < 0 => inner(m + 1, end)
            case x if x > 0 => inner(start, m - 1)
          }
        }
      }
      inner(0, nums.size - 1)
    }
  }

  def run() = {
    println(Solution.search(Array(-1, 0, 3, 5, 9, 12), 9) == 4)
    println(Solution.search(Array(-1, 0, 3, 5, 9, 12), 2) == -1)
    println(Solution.search(Array(-1, 0, 3, 5, 9, 12), 13) == -1)
  }
}
