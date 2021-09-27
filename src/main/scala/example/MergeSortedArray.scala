package example

/** Merge Sorted Array
  * You are given two integer arrays nums1 and nums2, sorted in non-decreasing order, and two integers m and n, representing the number of elements in nums1 and nums2 respectively.
  *
  * Merge nums1 and nums2 into a single array sorted in non-decreasing order.
  *
  * The final sorted array should not be returned by the function, but instead be stored inside the array nums1. To accommodate this, nums1 has a length of m + n, where the first m elements denote the elements that should be merged, and the last n elements are set to 0 and should be ignored. nums2 has a length of n.
  * Constraints:
  *
  * nums1.length == m + n
  * nums2.length == n
  * 0 <= m, n <= 200
  * 1 <= m + n <= 200
  * -109 <= nums1[i], nums2[j] <= 109
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-easy/96/sorting-and-searching/587/
  */
trait MergeSortedArray {
  object Solution {
    def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
      //binary search to find position which nums1[position] > n && nums1[position-1] <= n
      // def findIdx(n: Int, start: Int, end: Int): Int = {
      //   println(s"finding $n within $start and $end")
      //   val m = (start + end) / 2
      //   val mv = nums1(m)
      //   if (mv > n && nums1(m - 1) <= n) m
      //   else if (mv == n) findIdx(n, m + 1, end)
      //   else if (mv > n) findIdx(n, start, m)
      //   else findIdx(n, m + 1, end)
      // }
      // if (m == 0) nums2.copyToArray(nums1)
      // else {
      //   nums2.foreach { n =>
      //     val idx = findIdx(n, 0, nums1.length - 1)
      //     println(s"got idx $idx for $n")
      //     nums1.copyToArray(nums1, idx + 1, nums1.length - idx)
      //   }
      // }

      merge(nums1, m, nums2, n - 1)

    }

    def find(nums: Array[Int], n: Int, start: Int, end: Int): Int = {
      println(s"finding $n within $start and $end")
      (end - start) match {
        case 0 => if ((nums(start) - n) <= 0) start + 1 else start
        case 1 =>
          if (nums(start) == nums(end)) if (nums(start) <= n) end + 1 else -1
          else if (nums(start) <= n && nums(end) > n) end
          else -1
        case _ =>
          val m = (start + end) / 2
          val mv = nums(m)
          if ((mv - n) <= 0) find(nums, n, m + 1, end)
          else find(nums, n, start, m)
      }
    }
  }
  def run() = {
    // Solution.merge(Array(1, 2, 3, 0, 0, 0), 3, Array(2, 5, 6), 3)
    println(Solution.find(Array(1, 2, 3, 0, 0, 0), 2, 0, 2) == 2)
    println(Solution.find(Array(1, 2, 3, 0, 0, 0), 3, 0, 2) == 3)
    println(Solution.find(Array(1, 2, 3, 5, 0, 0), 3, 0, 3) == 3)
    println(Solution.find(Array(1, 1, 1, 1, 0), 1, 0, 3) == 4)
    println(Solution.find(Array(1, 0, 0), 1, 0, 0) == 1)
  }
}
