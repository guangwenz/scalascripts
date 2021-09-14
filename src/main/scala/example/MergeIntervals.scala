package example

/** Given an array of intervals where intervals[i] = [starti, endi], merge all overlapping intervals, and return an array of the non-overlapping intervals that cover all the intervals in the input.
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/110/sorting-and-searching/803/
  */
trait MergeIntervals {

  object Solution {
    def isOverlap(a: Array[Int], b: Array[Int]): Boolean =
      (a(0) >= b(0) && a(1) <= b(1)) || (b(0) >= a(0) && b(1) <= a(1)) || (a(
        1
      ) >= b(0) && a(0) <= b(1))

    def mergeItem(a: Array[Int], b: Array[Int]): Array[Int] = Array(
      math.min(a(0), b(0)),
      math.max(a(1), b(1))
    )

    def show(in: Array[Array[Int]]): String =
      in.map(_.mkString(",")).mkString("|")

    def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
      def merge(item: Array[Int], in: Array[Array[Int]]): Array[Array[Int]] = {
        in match {
          case Array() => Array(item)
          case Array(h, tail @ _*) =>
            if (isOverlap(item, h))
              merge(mergeItem(item, h), tail.toArray)
            else
              h +: merge(item, tail.toArray)
        }
      }

      def loop(
          in: Array[Array[Int]],
          acc: Array[Array[Int]]
      ): Array[Array[Int]] = in match {
        case Array(h) => Array(h)
        case Array(h, t @ _*) =>
          merge(h, loop(t.toArray, acc))
      }

      loop(intervals, Array.empty)
    }
  }

  def run() = {
    Solution.merge(Array(Array(1, 3), Array(2, 6), Array(8, 10), Array(15, 18)))
    Solution.merge(Array(Array(1, 4), Array(4, 5)))
    Solution.merge(Array(Array(1, 4), Array(0, 4)))
    Solution.merge(Array(Array(1, 4), Array(0, 0)))
    Solution.merge(Array(Array(1, 4), Array(0, 1)))
    Solution.merge(
      Array(Array(2, 3), Array(4, 5), Array(6, 7), Array(8, 9), Array(1, 10))
    )
    Solution.merge(Array(Array(1, 3), Array(2, 6), Array(8, 10), Array(15, 18)))
  }
}
