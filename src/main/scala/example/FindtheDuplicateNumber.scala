package example

/** Find the Duplicate Number
  *
  * Given an array of integers nums containing n + 1 integers where each integer is in the range [1, n] inclusive.
  *
  * There is only one repeated number in nums, return this repeated number.
  *
  * You must solve the problem without modifying the array nums and uses only constant extra space.
  *
  * Constraints:
  *
  * 1 <= n <= 10^5
  * nums.length == n + 1
  * 1 <= nums[i] <= n
  * All the integers in nums appear only once except for precisely one integer which appears two or more times.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/116/array-and-strings/834/
  */
trait FindtheDuplicateNumber {

  /** merge sort with tail recursion
    * divide and conquer algo
    * divide the list into 2 and sort them, then merge them
    */
  object MergeSort {
    // @annotation.tailrec
    def merge(l: List[Int], r: List[Int], acc: List[Int]): List[Int] =
      // (l, r) match {
      //   case (Nil, rs) => acc ::: rs
      //   case (ls, Nil) => acc ::: ls
      //   case (lh :: lt, rh :: rt) =>
      //     if (lh < rh) merge(lt, r, acc :+ lh)
      //     else merge(l, rt, acc :+ rh)
      // }
      l ::: r

    def sort(in: List[Int]): List[Int] = {
      in match {
        case Nil         => Nil
        case head :: Nil => in
        case _ =>
          val middle = in.size / 2
          val (left, right) = in.splitAt(middle)
          merge(sort(left), sort(right), Nil)
      }
    }
  }

  object Solution {
    def findDuplicate(nums: Array[Int]): Int = {
      val sorted = MergeSort.sort(nums.toList)
      // val sorted = nums.sorted
      println(sorted.mkString(","))
      // (1 until sorted.length).foreach { i =>
      //   if (sorted(i) == sorted(i - 1)) return sorted(i)
      // }
      return -1

    }
  }

  def run() = {
    // println(Solution.findDuplicate(Array(1, 3, 4, 2, 2)) == 2)
    // println(Solution.findDuplicate(Array(3, 1, 3, 4, 2)) == 3)
    // println(Solution.findDuplicate(Array(1, 1)) == 1)
    // println(Solution.findDuplicate(Array(1, 1, 2)) == 1)
    println(Solution.findDuplicate(Array.range(0, 100000)) == 1)
  }
}
