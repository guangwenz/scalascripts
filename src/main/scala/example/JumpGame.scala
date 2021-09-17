package example

/** You are given an integer array nums. You are initially positioned at the array's first index, and each element in the array represents your maximum jump length at that position.
  * Return true if you can reach the last index, or false otherwise.
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/111/dynamic-programming/807/
  */

trait JumpGame {
  object Solution {
    def canJump(nums: Array[Int]): Boolean = {
      nums match {
        case Array(_) => true
        case _ =>
          (1 until nums.length).forall { m =>
            var idx = 1
            ((m - 1) to 0 by -1).exists { i =>
              val ret = nums(i) >= idx
              idx = idx + 1
              ret
            }
          }
      }
    }
  }

  def run() = {
    println(Solution.canJump(Array(0)) == true)
    println(Solution.canJump(Array(1)) == true)
    println(Solution.canJump(Array(0, 2, 3)) == false)
    println(Solution.canJump(Array(2, 0, 0)) == true)
    println(Solution.canJump(Array(2, 3, 1, 1, 4)) == true)
    println(Solution.canJump(Array(3, 2, 1, 0, 4)) == false)
    println(Solution.canJump(Array(3, 2, 1)) == true)
    println(
      Solution.canJump(
        Array(3, 2, 1, 32, 3, 2, 2, 1, 3, 34, 1, 2, 0, 32, 1, 2, 2, 3, 4, 89, 3,
          2, 1, 32, 3, 2, 2, 1, 3)
      ) == true
    )
    println(
      Solution.canJump(
        Array(8, 2, 4, 4, 4, 9, 5, 2, 5, 8, 8, 0, 8, 6, 9, 1, 1, 6, 3, 5, 1, 2,
          6, 6, 0, 4, 8, 6, 0, 3, 2, 8, 7, 6, 5, 1, 7, 0, 3, 4, 8, 3, 5, 9, 0,
          4, 0, 1, 0, 5, 9, 2, 0, 7, 0, 2, 1, 0, 8, 2, 5, 1, 2, 3, 9, 7, 4, 7,
          0, 0, 1, 8, 5, 6, 7, 5, 1, 9, 9, 3, 5, 0, 7, 5)
      ) == true
    )
  }
}
