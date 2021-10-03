package example

import java.util.Arrays

/** Sliding Window Maximum
  * You are given an array of integers nums, there is a sliding window of size k which is moving from the very left of the array to the very right. You can only see the k numbers in the window. Each time the sliding window moves right by one position.
  *
  * Return the max sliding window.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 10^5
  * -10^4 <= nums[i] <= 10^4
  * 1 <= k <= nums.length
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/116/array-and-strings/837/
  */
trait SlidingWindowMaximum {
  object Solution {
    def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] = {
      val deque = collection.mutable.ArrayDeque.empty[Int]

      var lastIdx = 1
      val ret = Array.fill(nums.length - k + 1)(0)

      (0 until k).foreach { i =>
        while (deque.nonEmpty && nums(i) >= nums(deque.head)) {
          deque.removeHead()
        }
        deque.prepend(i)
      }

      ret(0) = nums(deque.last)
      (k until nums.length).foreach { i =>
        while (deque.nonEmpty && nums(i) >= nums(deque.head)) {
          deque.removeHead()
        }

        while (deque.nonEmpty && deque.last <= i - k) {
          deque.removeLast()
        }
        deque.prepend(i)

        ret(lastIdx) = nums(deque.last)
        lastIdx += 1
      }
      ret
    }
  }

  def run() = {
    println(
      Solution
        .maxSlidingWindow(Array(1, 3, -1, -3, 5, 3, 6, 7), 3)
        // .mkString(",")
        .sameElements(Array(3, 3, 5, 5, 6, 7))
    )
    println(
      Solution
        .maxSlidingWindow(Array(9, 10, 9, -7, -4, -8, 2, -6), 5)
        // .mkString(",")
        .sameElements(Array(10, 10, 9, 2))
    )
    println(Solution.maxSlidingWindow(Array(1), 1).sameElements(Array(1)))
    println(
      Solution.maxSlidingWindow(Array(1, -1), 1).sameElements(Array(1, -1))
    )
    println(Solution.maxSlidingWindow(Array(9, 11), 2).sameElements(Array(11)))
    println(Solution.maxSlidingWindow(Array(4, -2), 2).sameElements(Array(4)))
  }
}
