package medium

/** Ugly Number II
  * An ugly number is a positive integer whose prime factors are limited to 2, 3, and 5.
  *
  * Given an integer n, return the nth ugly number.
  *
  * Constraints:
  *
  * 1 <= n <= 1690
  *
  * https://leetcode.com/problems/ugly-number-ii/
  */
trait UglyNumberII {
  object Solution {
    def nthUglyNumber(n: Int): Int = {
      var (next2M, next3M, next5M) = (2, 3, 5)
      var (i2, i3, i5) = (0, 0, 0)
      var next = 1
      val uglyNums = Array.fill(n)(0)
      uglyNums(0) = 1

      (1 until n).foreach { i =>
        next = List(next2M, next3M, next5M).min
        uglyNums(i) = next

        if (next == next2M) {
          i2 += 1
          next2M = uglyNums(i2) * 2
        }
        if (next == next3M) {
          i3 += 1
          next3M = uglyNums(i3) * 3
        }
        if (next == next5M) {
          i5 += 1
          next5M = uglyNums(i5) * 5
        }
      }
      println(s"${uglyNums.mkString(",")}")
      next
    }
  }
  def run() = {
    Solution.nthUglyNumber(20)
    println(Solution.nthUglyNumber(10) == 12)
    println(Solution.nthUglyNumber(1) == 1)
  }
}
