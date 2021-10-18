package easy

/** Search Insert Position
  *
  * Given a sorted array of distinct integers and a target value, return the index if the target is found. If not, return the index where it would be if it were inserted in order.
  *
  * You must write an algorithm with O(log n) runtime complexity.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 10^4
  * -10^4 <= nums[i] <= 10^4
  * nums contains distinct values sorted in ascending order.
  * -10^4 <= target <= 10^4
  */
trait SearchInsertPosition {
  object Solution {
    def searchInsert(nums: Array[Int], target: Int): Int = {
      def inner(start: Int, end: Int, fn: Int => Boolean): Int = {
        if (start > end) -1
        else {
          val m = start + (end - start) / 2
          if (fn(m)) m
          else if (nums(m) < target) inner(m + 1, end, fn)
          else inner(start, m - 1, fn)
        }
      }

      if (nums.head > target) 0
      else if (nums.last < target) nums.length
      else {
        val n = inner(
          0,
          nums.length - 1,
          i => nums(i) == target || (nums(i) < target && nums(i + 1) > target)
        )
        if (nums(n) == target) n else n + 1
      }
    }
  }
  def run() = {
    println(Solution.searchInsert(Array(1, 3, 5, 6), 5) == 2)
    println(Solution.searchInsert(Array(1, 3, 5, 6), 2) == 1)
    println(Solution.searchInsert(Array(1, 3, 5, 6), 7) == 4)
    println(Solution.searchInsert(Array(1, 3, 5, 6), 0) == 0)
    println(Solution.searchInsert(Array(1), 0) == 0)
  }
}
