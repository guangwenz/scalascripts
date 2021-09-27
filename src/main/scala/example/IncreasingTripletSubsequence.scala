package example

/** Given an integer array nums, return true if there exists a triple of indices (i, j, k) such that i < j < k and nums[i] < nums[j] < nums[k]. If no such indices exists, return false.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 5 * 10^5
  * -2^31 <= nums[i] <= 2^31 - 1
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/103/array-and-strings/781/
  */
trait IncreasingTripletSubsequence {

  object Solution {
    def increasingTriplet(nums: Array[Int]): Boolean = {
      def increasingOne(start: Int): Boolean =
        (start + 1 until nums.length).exists(i => nums(i) > nums(start))
      def increasingDouble(start: Int): Boolean =
        (start + 1 until nums.length).exists(i =>
          nums(i) > nums(start) && increasingOne(i)
        )

      (0 until nums.length).exists { i => increasingDouble(i) }
    }
  }
  def run() = {
    println(Solution.increasingTriplet(Array(1, 2, 3, 4, 5)) == true)
    println(Solution.increasingTriplet(Array(5, 4, 3, 2, 1)) == false)
    println(Solution.increasingTriplet(Array(2, 1, 5, 0, 4, 6)) == true)
    println(Solution.increasingTriplet(Array(20, 100, 10, 12, 5, 13)) == true)
    println(Solution.increasingTriplet(Array(1, 5, 0, 4, 1, 3)) == true)
  }
}
