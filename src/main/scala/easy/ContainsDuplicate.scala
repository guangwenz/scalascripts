package easy

/** Contains Duplicate
  * Given an integer array nums, return true if any value appears at least twice in the array, and return false if every element is distinct.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 10^5
  * -10^9 <= nums[i] <= 10^9
  *
  * https://leetcode.com/problems/contains-duplicate/
  */
trait ContainsDuplicate {
  object Solution {
    def containsDuplicate(nums: Array[Int]): Boolean = {
      nums.foldLeft(Map.empty[Int, Int]) { case (s, num) =>
        s.get(num) match {
          case None    => s + (num -> 1)
          case Some(v) => return true
        }
      }
      false
    }
  }

  def run() = {
    println(Solution.containsDuplicate(Array(1, 2, 3, 1)) == true)
    println(Solution.containsDuplicate(Array(1, 2, 3, 4)) == false)
    println(
      Solution.containsDuplicate(Array(1, 1, 1, 3, 3, 4, 3, 2, 4, 2)) == true
    )
  }
}
