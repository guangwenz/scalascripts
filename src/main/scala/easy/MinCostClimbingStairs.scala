package easy

/** Min Cost Climbing Stairs
  *
  * You are given an integer array cost where cost[i] is the cost of ith step on a staircase. Once you pay the cost, you can either climb one or two steps.
  *
  * You can either start from the step with index 0, or the step with index 1.
  *
  * Return the minimum cost to reach the top of the floor.
  * Constraints:
  *
  * 2 <= cost.length <= 1000
  * 0 <= cost[i] <= 999
  * https://leetcode.com/problems/min-cost-climbing-stairs/
  */
trait MinCostClimbingStairs {
  object Solution {
    def minCostClimbingStairs(cost: Array[Int]): Int = {
      var (n1, n2) = (0, cost.head)
      (2 to cost.length).foreach { i =>
        val t = n1
        n1 = n2
        n2 = math.min(t, n1) + cost(i - 1)
      }
      math.min(n1, n2)
    }
  }

  def run() = {
    println(Solution.minCostClimbingStairs(Array(10, 15, 20)) == 15)
    println(
      Solution.minCostClimbingStairs(
        Array(1, 100, 1, 1, 1, 100, 1, 1, 100, 1)
      ) == 6
    )
  }
}
