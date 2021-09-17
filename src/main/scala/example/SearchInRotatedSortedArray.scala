package example

/** Search in Rotated Sorted Array
  *
  * There is an integer array nums sorted in ascending order (with distinct values).
  *
  * Prior to being passed to your function, nums is rotated at an unknown pivot index k (0 <= k < nums.length) such that the resulting array is [nums[k], nums[k+1], ..., nums[n-1], nums[0], nums[1], ..., nums[k-1]] (0-indexed). For example, [0,1,2,4,5,6,7] might be rotated at pivot index 3 and become [4,5,6,7,0,1,2].
  *
  * Given the array nums after the rotation and an integer target, return the index of target if it is in nums, or -1 if it is not in nums.
  *
  * You must write an algorithm with O(log n) runtime complexity.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 5000
  * -104 <= nums[i] <= 104
  * All values of nums are unique.
  * nums is guaranteed to be rotated at some pivot.
  * -104 <= target <= 104
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/110/sorting-and-searching/804/
  */
trait SearchInRotatedSortedArray {
  object Solution {
    def search(nums: Array[Int], target: Int): Int = {
      def loop(i: Int, j: Int): Int = {
        // println(s"finding $target between $i and $j")
        if (i < 0 || j > nums.length - 1 || j < i) -1
        else {
          val p = (i + j) / 2
          val pv = nums(p)
          if (target > pv) loop(p + 1, j)
          else if (target < pv) loop(i, p - 1)
          else p
        }
      }

      def first(i: Int, j: Int): Int = {
        // println(s"i $i j $j")
        if (j < i) 0
        else {
          val p = (i + j) / 2
          val pv = nums(p)
          if (p == 0) {
            if (nums(0) > nums(1)) 1 else 0
          } else if (pv < nums(p - 1)) p
          else if (pv > nums.head) first(p + 1, j)
          else first(i, p - 1)
        }
      }

      val p = nums match {
        case Array(h)    => 0
        case Array(h, t) => if (t > h) 0 else 1
        case _           => first(0, nums.size - 1)
      }

      // println(p)
      if (p > 0) {
        if (target < nums.head) loop(p, nums.length - 1)
        else loop(0, p)
      } else loop(0, nums.length - 1)
    }
  }

  def run() = {
    println(Solution.search(Array(4, 5, 6, 7, 0, 1, 2), 0) == 4)
    println(Solution.search(Array(4, 5, 6, 7, 0, 1, 2), 3) == -1)
    println(Solution.search(Array(1), 0) == -1)
    println(Solution.search(Array(1), 1) == 0)
    println(Solution.search(Array(1, 3), 0) == -1)
    println(Solution.search(Array(1, 3), 3) == 1)
    println(Solution.search(Array(3, 1), 1) == 1)
    println(Solution.search(Array(5, 1, 2, 3, 4), 1) == 1)
    println(Solution.search(Array(1, 2, 3, 4, 5), 5) == 4)
  }
}
