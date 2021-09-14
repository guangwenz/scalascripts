package example

/** Given an integer array nums and an integer k, return the kth largest element in the array.
  * Note that it is the kth largest element in the sorted order, not the kth distinct element.
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/110/sorting-and-searching/800/
  */
trait KthLargestElementInAnArray {
  object Solution {
    def findKthLargest(nums: Array[Int], k: Int): Int = {
      val sorted = nums.sortBy(i => -i)
      sorted(k - 1)
    }
  }

  def run() = {
    println(Solution.findKthLargest(Array(3, 2, 1, 5, 6, 4), 2) == 5)
    println(Solution.findKthLargest(Array(3, 2, 3, 1, 2, 4, 5, 5, 6), 4) == 4)
  }
}
