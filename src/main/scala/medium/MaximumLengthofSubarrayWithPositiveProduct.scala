package medium

/** Given an array of integers nums, find the maximum length of a subarray where the product of all its elements is positive.
  *
  * A subarray of an array is a consecutive sequence of zero or more values taken out of that array.
  *
  * Return the maximum length of a subarray with positive product.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 10^5
  * -10^9 <= nums[i] <= 10^9
  *
  * https://leetcode.com/problems/maximum-length-of-subarray-with-positive-product/
  */
trait MaximumLengthofSubarrayWithPositiveProduct {
  object Solution2 {
    def getMaxLen(nums: Array[Int]): Int = {
      def maxLen(start: Int): Int = {
        val p = (start + 1 until nums.length)
          .foldLeft(
            (nums(start), if (nums(start) > 0) 1 else 0)
          ) { case ((product, len), i) =>
            (product, nums(i)) match {
              case (l, r) if l == 0 || r == 0 => (0, len)
              case (l, r) if (l > 0 && r > 0) || (l < 0 && r < 0) =>
                (1, i - start + 1)
              case _ => (-1, len)
            }
          }
        // println(p)
        p._2
      }
      var result = 0
      for {
        i <- 0 until nums.length
        max = maxLen(i)
      } yield {
        // println(s"max is $max")
        result = math.max(max, result)
      }
      result
    }
  }

  object Solution {
    def getMaxLen(nums: Array[Int]): Int = {
      def maxLen(start: Int, end: Int): Int = {
        val neg = (start to end).filter(nums(_) < 0)
        if (neg.size % 2 == 0) end - start + 1
        else {
          //   println(
          //     s"net pos $neg for between $start:${nums(start)} and $end:${nums(end)}"
          //   )
          neg.foldLeft(0) { case (s, i) =>
            math.max(s, math.max(end - i, i - start))
          }
        }
      }
      val zeros = (0 until nums.length).filter(nums(_) == 0)
      if (zeros.isEmpty) maxLen(0, nums.length - 1)
      else {
        val init = zeros
          .foldLeft((-1, 0)) { case ((start, ret), i) =>
            val localMax = maxLen(start + 1, i - 1)
            (i, math.max(localMax, ret))
          }
          ._2
        val lastMax = maxLen(zeros.last + 1, nums.length - 1)
        // println(s"init $init and last $lastMax for ${nums.mkString(",")}")
        math.max(init, lastMax)
      }
    }
  }
  def run() = {
    println(Solution.getMaxLen(Array(1, -2, -3, 4)) == 4)
    println(Solution.getMaxLen(Array(0, 1, -2, -3, -4)) == 3)
    println(Solution.getMaxLen(Array(-1, -2, -3, 0, 1)) == 2)
    println(Solution.getMaxLen(Array(-1, 2)) == 1)
    println(Solution.getMaxLen(Array(1000000000, 1000000000)) == 2)
    println(Solution.getMaxLen(Array(1, 2, 3, 5, -6, 4, 0, 10)) == 4)
    println(Solution.getMaxLen(Array(-16, 0, -5, 2, 2, -13, 11, 8)) == 6)
    println(Solution.getMaxLen(Array(0, 0, 0, 0, 0)) == 0)
    println(
      Solution.getMaxLen(
        Array(5, -20, -20, -39, -5, 0, 0, 0, 36, -32, 0, -7, -10, -7, 21, 20,
          -12, -34, 26, 2)
      ) == 8
    )
    println(
      Solution.getMaxLen(Array(17, 0, 17, 0, 5, -10, -15, 13, -14, -3)) == 6
    )
  }

}
