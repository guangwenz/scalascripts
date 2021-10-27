package medium

/** Best Sightseeing Pair
  * You are given an integer array values where values[i] represents the value of the ith sightseeing spot. Two sightseeing spots i and j have a distance j - i between them.
  *
  * The score of a pair (i < j) of sightseeing spots is values[i] + values[j] + i - j: the sum of the values of the sightseeing spots, minus the distance between them.
  *
  * Return the maximum score of a pair of sightseeing spots.
  *
  * Constraints:
  *
  * 2 <= values.length <= 5 * 10^4
  * 1 <= values[i] <= 1000
  * https://leetcode.com/problems/best-sightseeing-pair/
  */
trait BestSightseeingPair {
  object Solution {
    def maxScoreSightseeingPair(values: Array[Int]): Int = {
      var result = 0
      var max = values(0)
      for {
        j <- 1 until values.length
      } yield {
        max -= 1
        result = math.max(values(j) + max, result)
        max = math.max(values(j), max)
      }
      result
    }
  }

  def run() = {
    println(Solution.maxScoreSightseeingPair(Array(8, 1, 5, 2, 6)) == 11)
    println(Solution.maxScoreSightseeingPair(Array(1, 2)) == 2)
    println(Solution.maxScoreSightseeingPair(Array(1, 3, 5)) == 7)
  }
}
