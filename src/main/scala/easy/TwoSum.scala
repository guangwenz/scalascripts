package easy

/** Two Sum
  *
  * Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.
  *
  * You may assume that each input would have exactly one solution, and you may not use the same element twice.
  *
  * You can return the answer in any order.
  *
  * Constraints:
  *
  * 2 <= nums.length <= 10^4
  * -10^9 <= nums[i] <= 10^9
  * -10^9 <= target <= 10^9
  * Only one valid answer exists.
  *
  * https://leetcode.com/problems/two-sum/
  */
trait TwoSum {
  object Solution {
    def twoSum(nums: Array[Int], target: Int): Array[Int] = {
      val sortedWithIdx = nums.zipWithIndex.sortBy(_._1)
      var (l, r) = (0, sortedWithIdx.length - 1)

      while (l <= r) {
        val sum = sortedWithIdx(l)._1 + sortedWithIdx(r)._1
        val idx = Array(sortedWithIdx(l)._2, sortedWithIdx(r)._2)
        if (sum == target) {
          return idx
        } else if (sum < target) {
          l += 1
        } else r -= 1
      }
      Array.empty
    }
  }

  def run() = {
    println(Solution.twoSum(Array(2, 7, 11, 15), 9).sameElements(Array(0, 1)))
    println(Solution.twoSum(Array(3, 2, 4), 6).sameElements(Array(1, 2)))
    println(Solution.twoSum(Array(3, 3), 6).sameElements(Array(0, 1)))
  }
}
