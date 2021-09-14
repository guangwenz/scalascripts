package example

/** Given an array of integers nums sorted in ascending order, find the starting and ending position of a given target value.
  * If target is not found in the array, return [-1, -1].
  * You must write an algorithm with O(log n) runtime complexity.
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/110/sorting-and-searching/802/
  */
trait SearchForARange {
  object Solution {
    def searchRange(nums: Array[Int], target: Int): Array[Int] = {
      def findRange(idx: Int): Array[Int] = {
        Array(
          (math.max(idx, 0) to 0 by -1)
            .takeWhile(nums(_) == target)
            .lastOption
            .getOrElse(idx),
          (math.min(idx + 1, nums.size - 1) until nums.size)
            .takeWhile(nums(_) == target)
            .lastOption
            .getOrElse(idx)
        )

      }
      def findLast(nums: Array[(Int, Int)]): Option[Int] = {
        nums match {
          case Array()       => None
          case Array((h, m)) => if (h == target) Some(m) else None
          case _ =>
            val m = nums.size / 2
            val (v, i) = nums(m)
            val (l, r) = nums.splitAt(m)
            v match {
              case x if x == target => Some(i)
              case x if x < target  => findLast(r)
              case x if x > target  => findLast(l)
            }
        }
      }
      findLast(nums.zipWithIndex) match {
        case None    => Array(-1, -1)
        case Some(v) => findRange(v)
      }
    }
  }

  def run() = {
    println(
      Solution
        .searchRange(Array(5, 7, 7, 8, 8, 10), 8) sameElements (Array(3, 4))
    )
    println(
      Solution
        .searchRange(Array(5, 7, 7, 8, 8, 10), 6) sameElements (Array(-1, -1))
    )
    println(
      Solution
        .searchRange(Array(1), 1) sameElements (Array(0, 0))
    )
    println(
      Solution
        .searchRange(Array(), 0) sameElements (Array(-1, -1))
    )
    println(
      Solution
        .searchRange(Array(1, 2, 3, 3, 3, 3, 4, 5, 9), 3) sameElements (Array(
        2,
        5
      ))
    )
  }
}
