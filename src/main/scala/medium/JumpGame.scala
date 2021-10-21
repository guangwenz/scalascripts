package medium

/** Jump Game
  *
  * You are given an integer array nums. You are initially positioned at the array's first index, and each element in the array represents your maximum jump length at that position.
  *
  * Return true if you can reach the last index, or false otherwise.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 10^4
  * 0 <= nums[i] <= 10^5
  *
  * https://leetcode.com/problems/jump-game/
  */
trait JumpGame {

  /** we only care about 0 here, everytime we see a zero, if it's at end, we make sure we can reach, if not we make it can be skipped
    */
  object Solution {
    def canJump(nums: Array[Int]): Boolean = {
      nums match {
        case Array()  => true
        case Array(h) => true
        case _ =>
          for {
            i <- nums.indices
            n = nums(i)
            if n == 0
          } yield {
            val canReach = {
              if (i == nums.length - 1) (i - 1 to 0 by -1).exists { j =>
                nums(j) > (i - j - 1)
              }
              else
                (i - 1 to 0 by -1).exists { j =>
                  nums(j) > (i - j)
                }
            }
            if (!canReach) return false
          }
          true
      }
    }
  }
  object Solution2 {
    def canJump(nums: Array[Int]): Boolean = {
      val memo = collection.mutable.Map.empty[Int, Boolean]
      def canJump(start: Int): Boolean = {
        if (memo.contains(start)) memo(start)
        else {
          val ret =
            if (start >= nums.length - 1) true
            else {
              (1 to nums(start)).exists(i => canJump(start + i))
            }
          memo.put(start, ret)
          ret
        }
      }
      canJump(0)
    }
  }

  def run() = {
    println(Solution.canJump(Array(0)) == true)
    println(Solution.canJump(Array(2, 0, 0)) == true)
    println(Solution.canJump(Array(2, 3, 1, 1, 4)) == true)
    println(Solution.canJump(Array(3, 2, 1, 0, 4)) == false)
  }
}
