package easy

/** House Robber
  *
  * You are a professional robber planning to rob houses along a street. Each house has a certain amount of money stashed, the only constraint stopping you from robbing each of them is that adjacent houses have security systems connected and it will automatically contact the police if two adjacent houses were broken into on the same night.
  *
  * Given an integer array nums representing the amount of money of each house, return the maximum amount of money you can rob tonight without alerting the police.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 100
  * 0 <= nums[i] <= 400
  *
  * https://leetcode.com/problems/house-robber/
  * https://leetcode.com/explore/interview/card/top-interview-questions-easy/97/dynamic-programming/576/
  */
trait HouseRobber {
  object Solution {
    def rob(nums: Array[Int]): Int = {
      nums match {
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
  }
  object Solution2 {
    def rob(nums: Array[Int]): Int = {
      nums match {
        case Array(a)    => a
        case Array(a, b) => math.max(a, b)
        case _ =>
          val dp = Array.fill(nums.length)(0)
          val (a, b) = (nums(0), nums(1))
          dp(0) = a
          dp(1) = math.max(a, b)
          (2 until dp.length).foreach { i =>
            val v = nums(i)
            dp(i) = math.max(dp(i - 1), dp(i - 2) + v)
          }
          dp.max
      }
    }
  }

  def run() = {
    println(Solution.rob(Array(1, 2, 3, 1)) == 4)
    println(Solution.rob(Array(2, 7, 9, 3, 1)) == 12)
  }
}
