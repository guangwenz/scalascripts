package easy

/** Climbing Stairs
  *
  * You are climbing a staircase. It takes n steps to reach the top.
  *
  * Each time you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?
  *
  * Constraints:
  *
  * 1 <= n <= 45
  * https://leetcode.com/problems/climbing-stairs/
  */
trait ClimbingStairs {
  object Solution {
    def climbStairs(n: Int): Int = {
      //   n match {
      //     case 1 => 1
      //     case 2 => 2
      //     case _ => climbStairs(n - 1) + climbStairs(n - 2)
      //   }
      var (n1, n2) = (1, 2)
      n match {
        case 1 => 1
        case 2 => 2
        case _ =>
          (3 to n).foreach { _ =>
            val t = n1
            n1 = n2
            n2 = t + n1
          }
          n2
      }
    }
  }

  def run() = {
    println(Solution.climbStairs(2) == 2)
    println(Solution.climbStairs(3) == 3)
    println(Solution.climbStairs(4) == 5)
    println(Solution.climbStairs(5) == 8)
    println(Solution.climbStairs(6) == 13)
  }
}
