package example

/** Minimum Time Difference
  *
  * Given a list of 24-hour clock time points in "HH:MM" format, return the minimum minutes difference between any two time-points in the list.
  *
  * Constraints:
  *
  * 2 <= timePoints <= 2 * 104
  * timePoints[i] is in the format "HH:MM".
  *
  * https://leetcode.com/problems/minimum-time-difference/
  */
trait MinimumTimeDifference {
  object Solution {
    def toMinOffset(t: String): Int = {
      val Array(h, m) = t.split(":").map(_.toInt)
      h * 60 + m
    }
    def findMinDifference(timePoints: List[String]): Int = {
      val head = toMinOffset(timePoints.head)
      val tail = timePoints.tail
        .foldLeft((0, head, List.empty[Int])) { case ((days, lastMin, l), i) =>
          val minOffset = toMinOffset(i)
          val newDays = if (minOffset < lastMin) days + 1 else days
          (newDays, minOffset, (minOffset + newDays * 24 * 60) +: l)
        }
        ._3
      val sorted = (head +: tail)
      println(sorted)
      val ret = sorted.tail
        .foldLeft((sorted.head, Int.MaxValue)) { case ((last, s), i) =>
          (i, if (math.abs(i - last) < s) math.abs(i - last) else s)
        }
      println(ret)
      ret._2
    }
  }

  def run() = {
    println(Solution.findMinDifference(List("23:59", "00:00")) == 1)
    println(Solution.findMinDifference(List("00:00", "23:59", "00:00")) == 0)
  }
}
