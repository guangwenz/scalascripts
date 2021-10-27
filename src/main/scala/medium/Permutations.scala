package medium

/** Permutations
  * Given an array nums of distinct integers, return all the possible permutations. You can return the answer in any order.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 6
  * -10 <= nums[i] <= 10
  * All the integers of nums are unique.
  *
  * https://leetcode.com/problems/permutations/
  */
trait Permutations {
  object Solution {
    def permute(nums: Array[Int]): List[List[Int]] = {
      nums match {
        case Array() => Nil
        case Array(h) =>
          List(List(h))
        case _ =>
          for {
            i <- List.range(0, nums.length)
            n = nums(i)
            (l, r) = nums.splitAt(i)
            sub <- permute(l ++ r.tail)
          } yield n +: sub
      }
    }
  }

  def run() = {
    println(Solution.permute(Array(1, 2, 3)))
    println(Solution.permute(Array(0, 1)))
    println(Solution.permute(Array(1)))
  }
}
