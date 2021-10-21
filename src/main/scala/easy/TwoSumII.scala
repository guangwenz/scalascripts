package easy

/** Two Sum II - Input array is sorted
  *
  * Given a 1-indexed array of integers numbers that is already sorted in non-decreasing order, find two numbers such that they add up to a specific target number. Let these two numbers be numbers[index1] and numbers[index2] where 1 <= first < second <= numbers.length.
  *
  * Return the indices of the two numbers, index1 and index2, as an integer array [index1, index2] of length 2.
  *
  * The tests are generated such that there is exactly one solution. You may not use the same element twice.
  *
  * Constraints:
  *
  * 2 <= numbers.length <= 3 * 10^4
  * -1000 <= numbers[i] <= 1000
  * numbers is sorted in non-decreasing order.
  * -1000 <= target <= 1000
  * The tests are generated such that there is exactly one solution.
  *
  * https://leetcode.com/problems/two-sum-ii-input-array-is-sorted/
  */
trait TwoSumII {
  object Solution {
    def twoSum(numbers: Array[Int], target: Int): Array[Int] = {
      var (l, r) = (0, numbers.length - 1)
      while (l < r) {
        val sum = numbers(l) + numbers(r)
        if (sum == target) return Array(l + 1, r + 1)
        else if (sum < target) l += 1
        else if (sum > target) r -= 1
      }
      Array.empty
    }
  }

  def run() = {
    println(Solution.twoSum(Array(2, 7, 11, 15), 9).sameElements(Array(1, 2)))
    println(Solution.twoSum(Array(2, 3, 4), 6).sameElements(Array(1, 3)))
    println(Solution.twoSum(Array(-1, 0), -1).sameElements(Array(1, 2)))
  }
}
