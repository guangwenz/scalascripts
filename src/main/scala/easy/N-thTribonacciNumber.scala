package easy

/** N-th Tribonacci Number
  *
  * The Tribonacci sequence Tn is defined as follows:
  *
  * T0 = 0, T1 = 1, T2 = 1, and Tn+3 = Tn + Tn+1 + Tn+2 for n >= 0.
  *
  * Given n, return the value of Tn.
  *
  * Constraints:
  *
  * 0 <= n <= 37
  * The answer is guaranteed to fit within a 32-bit integer, ie. answer <= 2^31 - 1.
  *
  * https://leetcode.com/problems/n-th-tribonacci-number/
  */
trait `N-thTribonacciNumber` {
  object Solution {
    def tribonacci(n: Int): Int = {
      n match {
        case 0 => 0
        case 1 => 1
        case 2 => 1
        case _ =>
          (3 to n)
            .foldLeft((0, 1, 1)) { case ((s1, s2, s3), n) =>
              (s2, s3, s1 + s2 + s3)
            }
            ._3
      }
    }
  }

  def run() = {
    println(Solution.tribonacci(4) == 4)
    println(Solution.tribonacci(25) == 1389537)
  }
}
