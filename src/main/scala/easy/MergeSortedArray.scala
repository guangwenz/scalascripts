package easy

/** Merge Sorted Array
  *
  * You are given two integer arrays nums1 and nums2, sorted in non-decreasing order, and two integers m and n, representing the number of elements in nums1 and nums2 respectively.
  *
  * Merge nums1 and nums2 into a single array sorted in non-decreasing order.
  *
  * The final sorted array should not be returned by the function, but instead be stored inside the array nums1. To accommodate this, nums1 has a length of m + n, where the first m elements denote the elements that should be merged, and the last n elements are set to 0 and should be ignored. nums2 has a length of n.
  *
  * Constraints:
  *
  * nums1.length == m + n
  * nums2.length == n
  * 0 <= m, n <= 200
  * 1 <= m + n <= 200
  * -10^9 <= nums1[i], nums2[j] <= 10^9
  *
  * https://leetcode.com/problems/merge-sorted-array/
  */
trait MergeSortedArray {
  object Solution {
    def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
      def insert(value: Int): Unit = {
        def insert(value: Int, idx: Int): Unit = {
          (m until idx by -1).foreach { i =>
            nums1(i) = nums1(i - 1)
          }
          nums1(idx) = value
        }
        if (value < nums1.head) insert(value, 0)
        else if (value > nums1(m - 1)) insert(value, m)
        else {
          insert(value, nums1.indexWhere(_ >= value))
        }
      }
      if (m == 0) {
        nums2.indices.foreach(i => nums1(i) = nums2(i))
      } else if (n == 0) {} else {
        insert(nums2.head)
        merge(nums1, m + 1, nums2.tail, n - 1)
      }
    }
  }

  def run() = {
    val input1 = Array(1, 2, 3, 0, 0, 0)
    Solution.merge(input1, 3, Array(2, 5, 6), 3)
    println(input1.sameElements(Array(1, 2, 2, 3, 5, 6)))
    val input2 = Array(1)
    Solution.merge(input2, 1, Array(), 0)
    println(input2.sameElements(Array(1)))
    val input3 = Array(0)
    Solution.merge(input3, 0, Array(1), 1)
    println(input3.sameElements(Array(1)))
    println(input2.sameElements(Array(1)))
    val input4 = Array(4, 5, 6, 0, 0, 0)
    Solution.merge(input4, 3, Array(1, 2, 3), 3)
    println(input4.sameElements(Array(1, 2, 3, 4, 5, 6)))
  }
}
