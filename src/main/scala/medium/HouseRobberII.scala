package medium

/** House Robber II
  *
  * You are a professional robber planning to rob houses along a street. Each house has a certain amount of money stashed. All houses at this place are arranged in a circle. That means the first house is the neighbor of the last one. Meanwhile, adjacent houses have a security system connected, and it will automatically contact the police if two adjacent houses were broken into on the same night.
  *
  * Given an integer array nums representing the amount of money of each house, return the maximum amount of money you can rob tonight without alerting the police.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 100
  * 0 <= nums[i] <= 1000
  *
  * https://leetcode.com/problems/house-robber-ii/
  */
trait HouseRobberII {
  object Solution {
    def rob(nums: Array[Int]): Int = {
      def robNotCircle(nums: Array[Int]): Int = {
        nums match {
          case Array()     => 0
          case Array(h)    => h
          case Array(h, t) => math.max(h, t)
          case Array(h, t, tail @ _*) =>
            var (n1, n2) = (h, math.max(h, t))
            (2 until nums.length).foreach { i =>
              val t = n1
              n1 = n2
              n2 = math.max(n1, t + nums(i))
            }
            n2
        }
      }
      nums match {
        case Array()     => 0
        case Array(h)    => h
        case Array(h, t) => math.max(h, t)
        case Array(h, t, tail @ _*) =>
          math.max(
            h + robNotCircle(tail.init.toArray),
            robNotCircle(t +: tail.toArray)
          )
      }
    }
  }

  def run() = {
    println(Solution.rob(Array(2, 3, 2)) == 3)
    println(Solution.rob(Array(1, 2, 3, 1)) == 4)
    println(Solution.rob(Array(1, 2, 3)) == 3)
    println(Solution.rob(Array(1, 1)) == 1)
  }
}
