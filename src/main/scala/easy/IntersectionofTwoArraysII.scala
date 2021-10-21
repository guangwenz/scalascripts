package easy

/** Intersection of Two Arrays II
  *
  * Given two integer arrays nums1 and nums2, return an array of their intersection. Each element in the result must appear as many times as it shows in both arrays and you may return the result in any order.
  *
  * Constraints:
  *
  * 1 <= nums1.length, nums2.length <= 1000
  * 0 <= nums1[i], nums2[i] <= 1000
  *
  * Follow up:
  *
  * What if the given array is already sorted? How would you optimize your algorithm?
  * What if nums1's size is small compared to nums2's size? Which algorithm is better?
  * What if elements of nums2 are stored on disk, and the memory is limited such that you cannot load all elements into the memory at once?
  *
  * https://leetcode.com/problems/intersection-of-two-arrays-ii/
  */
trait IntersectionofTwoArraysII {
  object Solution {
    def intersect(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
      def inner(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
        if (nums1.isEmpty || nums2.isEmpty) Array.empty
        else {
          (nums1.head, nums2.head) match {
            case (h1, h2) if h1 == h2 => h1 +: intersect(nums1.tail, nums2.tail)
            case (h1, h2) if h1 < h2  => intersect(nums1.tail, nums2)
            case _                    => intersect(nums1, nums2.tail)
          }
        }
      }
      inner(nums1.sorted, nums2.sorted)
    }
  }

  def run() = {
    println(
      Solution
        .intersect(Array(1, 2, 2, 1), Array(2, 2))
        .sameElements(Array(2, 2))
    )
    println(
      Solution
        .intersect(Array(4, 9, 5), Array(9, 4, 9, 8, 4))
        .sameElements(Array(4, 9))
    )
  }
}
