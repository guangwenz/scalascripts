package medium

/** You are given an integer array nums. You want to maximize the number of points you get by performing the following operation any number of times:
  *
  * Pick any nums[i] and delete it to earn nums[i] points. Afterwards, you must delete every element equal to nums[i] - 1 and every element equal to nums[i] + 1.
  * Return the maximum number of points you can earn by applying the above operation some number of times.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 2 * 10^4
  * 1 <= nums[i] <= 10^4
  *
  * https://leetcode.com/problems/delete-and-earn/
  */
trait DeleteandEarn {
  object Solution {
    def deleteAndEarn(nums: Array[Int]): Int = {
      val sorted = nums.sorted
      val dp = Array.fill(sorted.length)(0)
      dp(0) = sorted.head
      for {
        i <- 1 until sorted.length
        n = sorted(i)
      } yield {
        val last =
          if (sorted(i - 1) == n) dp(i - 1)
          else
            (i - 1 to 0 by -1)
              .findLast(j => sorted(j) < n - 1)
              .map(dp)
              .getOrElse(0)
        dp(i) = math.max(
          dp(i - 1),
          last + n
        )
      }
      println(dp.mkString(","))
      dp.last
    }
  }
  object Solution2 {
    def deleteAndEarn(nums: Array[Int]): Int = {
      val memo = collection.mutable.Map.empty[List[Int], Int]
      def deleteAndEarn(nums: List[Int]): Int = {
        if (memo.contains(nums)) memo(nums)
        else {
          val ret = nums match {
            case Nil      => 0
            case h :: Nil => h
            case _ =>
              var result = 0
              for {
                i <- List.range(0, nums.length)
                v = nums(i)
                newArr = List.range(i + 1, nums.length).collect {
                  case idx if nums(idx) != v + 1 && nums(idx) != v - 1 =>
                    nums(idx)
                }
              } yield {
                val ret = v + deleteAndEarn(newArr)
                if (ret > result) result = ret
              }
              result
          }
          memo.put(nums, ret)
          ret
        }
      }
      deleteAndEarn(nums.toList)
    }
  }

  def run() = {
    println(Solution.deleteAndEarn(Array(3, 4, 2)) == 6)
    println(Solution.deleteAndEarn(Array(2, 2, 3, 3, 3, 4)) == 9)
    println(
      Solution.deleteAndEarn(
        Array(8, 3, 4, 7, 6, 6, 9, 2, 5, 8, 2, 4, 9, 5, 9, 1, 5, 7, 1, 4)
      ) == 61
    )
    println(
      Solution.deleteAndEarn(
        Array(1, 8, 5, 9, 6, 9, 4, 1, 7, 3, 3, 6, 3, 3, 8, 2, 6, 3, 2, 2, 1, 2,
          9, 8, 7, 1, 1, 10, 6, 7, 3, 9, 6, 10, 5, 4, 10, 1, 6, 7, 4, 7, 4, 1,
          9, 5, 1, 5, 7, 5)
      ) == 138
    )
    println(
      Solution.deleteAndEarn(
        Array(12, 32, 93, 17, 100, 72, 40, 71, 37, 92, 58, 34, 29, 78, 11, 84,
          77, 90, 92, 35, 12, 5, 27, 92, 91, 23, 65, 91, 85, 14, 42, 28, 80, 85,
          38, 71, 62, 82, 66, 3, 33, 33, 55, 60, 48, 78, 63, 11, 20, 51, 78, 42,
          37, 21, 100, 13, 60, 57, 91, 53, 49, 15, 45, 19, 51, 2, 96, 22, 32, 2,
          46, 62, 58, 11, 29, 6, 74, 38, 70, 97, 4, 22, 76, 19, 1, 90, 63, 55,
          64, 44, 90, 51, 36, 16, 65, 95, 64, 59, 53, 93)
      ) == 138
    )
  }
}
