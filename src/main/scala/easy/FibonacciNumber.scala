package easy

/** Fibonacci Number
  * The Fibonacci numbers, commonly denoted F(n) form a sequence, called the Fibonacci sequence, such that each number is the sum of the two preceding ones, starting from 0 and 1. That is,
  *
  * F(0) = 0, F(1) = 1
  * F(n) = F(n - 1) + F(n - 2), for n > 1.
  * Given n, calculate F(n).
  *
  * Constraints:
  *
  * 0 <= n <= 30
  *
  * https://leetcode.com/problems/fibonacci-number/
  */
trait FibonacciNumber {
  object Solution {
    def fib(n: Int): Int = {
      n match {
        case 0 => 0
        case 1 => 1
        case _ =>
          (2 to n)
            .foldLeft((0, 1)) { case ((s1, s2), n) =>
              (s2, s1 + s2)
            }
            ._2
      }
    }
  }

  def run() = {
    println(Solution.fib(2) == 1)
    println(Solution.fib(3) == 2)
    println(Solution.fib(4) == 3)
  }
}
