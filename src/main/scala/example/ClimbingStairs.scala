package example

/** Climbing Stairs
  * You are climbing a staircase. It takes n steps to reach the top.
  *
  * Each time you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?
  *
  * Constraints:
  *
  * 1 <= n <= 45
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-easy/97/dynamic-programming/569/
  */
trait ClimbingStairs {
  object Solution {
    def climbStairs(n: Int): Int = {
      n match {
        case 0 => 0
        case 1 => 1
        case 2 => 2
        case _ =>
          val dp = Array.fill(n + 1)(0)
          dp(0) = 0
          dp(1) = 1
          dp(2) = 2
          (3 to n).foreach { i =>
            dp(i) = dp(i - 1) + dp(i - 2)
          }
          dp(n)
      }
    }
  }

  def run() = {
    println(Solution.climbStairs(2) == 2)
    println(Solution.climbStairs(3) == 3)
    println(Solution.climbStairs(4) == 5)
    println(Solution.climbStairs(5) == 8)
  }
}
