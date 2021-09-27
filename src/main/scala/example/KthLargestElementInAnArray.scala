package example

/** Given an integer array nums and an integer k, return the kth largest element in the array.
  * Note that it is the kth largest element in the sorted order, not the kth distinct element.
  *
  * Constraints:
  *
  * 1 <= k <= nums.length <= 10^4
  * -10^4 <= nums[i] <= 10^4
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/110/sorting-and-searching/800/
  */
trait KthLargestElementInAnArray {
  object Solution {
    def findKthLargest(nums: Array[Int], k: Int): Int = {
      // val sorted = nums.sortBy(i => -i)
      // sorted(k - 1)
      var ks = 0
      var i = 1
      var ret = nums.head
      val sorted = nums.sorted(Ordering.Int.reverse)
      while (ks < k - 1) {
        if (sorted(i) != sorted(i - 1)) {
          ks += 1
          ret = sorted(i)
        }
        i += 1
      }
      println(ret)
      ret
    }
  }

  def run() = {
    println(Solution.findKthLargest(Array(3, 2, 1, 5, 6, 4), 2) == 5)
    println(Solution.findKthLargest(Array(3, 2, 3, 1, 2, 4, 5, 5, 6), 4) == 4)
  }
}
