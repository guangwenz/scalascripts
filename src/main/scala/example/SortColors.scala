package example

/** Given an array nums with n objects colored red, white, or blue, sort them in-place so that objects of the same color are adjacent, with the colors in the order red, white, and blue.
  * We will use the integers 0, 1, and 2 to represent the color red, white, and blue, respectively.
  * You must solve this problem without using the library's sort function.
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/110/sorting-and-searching/798/
  */
trait SortColors {
  object Solution {
    def sortColors(nums: Array[Int]): Unit = {
      val (r, w, b) = nums.foldLeft((0, 0, 0)) { case ((r, w, b), i) =>
        i match {
          case 0 => (r + 1, w, b)
          case 1 => (r, w + 1, b)
          case 2 => (r, w, b + 1)
        }
      }
      (0 until r).map(i => nums(i) = 0)
      (r until r + w).map(i => nums(i) = 1)
      (r + w until r + w + b).map(i => nums(i) = 2)
    }
  }

  def run() = {
    val arr = Array(2, 0, 2, 1, 1, 0)
    Solution.sortColors(arr)
    println(arr.mkString(","))
  }
}
