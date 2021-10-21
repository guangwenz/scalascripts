package medium

/** Jump Game II
  * Given an array of non-negative integers nums, you are initially positioned at the first index of the array.
  *
  * Each element in the array represents your maximum jump length at that position.
  *
  * Your goal is to reach the last index in the minimum number of jumps.
  *
  * You can assume that you can always reach the last index.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 10^4
  * 0 <= nums[i] <= 1000
  */
trait JumpGameII {
  object Solution {
    def jump(nums: Array[Int]): Int = {
      val dp = Array.fill(nums.length)(0)
      (1 to nums.length - 1).foreach { i =>
        val p = for {
          j <- i - 1 to 0 by -1
          if nums(j) >= i - j
        } yield dp(j)
        if (p.nonEmpty) dp(i) = p.min + 1
      }
      dp.last
    }
  }

  def run() = {
    println(Solution.jump(Array(2, 3, 1, 1, 4)) == 2)
    println(Solution.jump(Array(2, 3, 0, 1, 4)) == 2)
  }
}
