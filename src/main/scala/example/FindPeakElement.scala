package example

/** Find Peak Element
  * A peak element is an element that is strictly greater than its neighbors.
  * Given an integer array nums, find a peak element, and return its index. If the array contains multiple peaks, return the index to any of the peaks.
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/110/sorting-and-searching/801/
  */
trait FindPeakElement {
  object Solution {
    def findPeakElement(nums: Array[Int]): Int = {
      def isPeak(idx: Int): Boolean = {
        val v = nums(idx)
        idx match {
          case 0                       => v > nums(1)
          case x if x == nums.size - 1 => v > nums(x - 1)
          case x                       => v > nums(x - 1) && v > nums(x + 1)
        }
      }

      def loop(idx: List[Int]): Option[Int] = idx match {
        case Nil         => None
        case head :: Nil => if (isPeak(head)) Some(head) else None
        case head :: tail =>
          if (isPeak(head)) Some(head)
          else {
            val (l, r) = idx.splitAt(idx.size / 2)
            loop(l) match {
              case None  => loop(r)
              case someV => someV
            }
          }

      }

      nums match {
        case Array(h)    => 0
        case Array(h, t) => if (h > t) 0 else 1
        case x           => loop((0 until nums.size).toList).getOrElse(-1)
      }
    }
  }

  def run() = {
    println(Solution.findPeakElement(Array(1, 2, 3, 1)) == 2)
    println(Solution.findPeakElement(Array(1, 3, 2, 1)) == 1)
    println(Solution.findPeakElement(Array(6, 5, 4, 3, 2, 3, 2)) == 0)
    println(Solution.findPeakElement(Array(1, 2)) == 1)
    println(Solution.findPeakElement(Array(1)) == 0)
    val ret2 = Solution.findPeakElement(Array(1, 2, 1, 3, 5, 6, 4))
    println(ret2 == 5 || ret2 == 1)
  }
}
