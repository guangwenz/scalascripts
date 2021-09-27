package example

/** Perfect Squares
  *
  * Given an integer n, return the least number of perfect square numbers that sum to n.
  *
  * A perfect square is an integer that is the square of an integer; in other words, it is the product of some integer with itself. For example, 1, 4, 9, and 16 are perfect squares while 3 and 11 are not.
  *
  * Constraints:
  *
  * 1 <= n <= 10^4
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/121/dynamic-programming/863/
  */
trait PerfectSquares {
  object Solution {
    def minumSum(in: List[Int], target: Int): Int = {
      val memo = collection.mutable.Map.empty[Int, Int]
      def inner(target: Int): Int = {
        if (memo.contains(target)) memo(target)
        else {
          val ret = target match {
            case 0          => 0
            case i if i < 0 => -1
            case _ =>
              in.map(i => inner(target - i))
                .collect {
                  case x if x >= 0 => x + 1
                }
                .sorted
                .headOption
                .getOrElse(-1)
          }
          memo.put(target, ret)
          ret
        }
      }
      inner(target)
    }
    def numSquares(n: Int): Int = {
      val x = math.sqrt(n)
      val xi = x.toInt
      if (x == xi) 1
      else {
        minumSum((1 to xi).map(i => i * i).toList, n)
      }
    }
  }

  def run() = {
    // println(Solution.numSquares(12))
    println(Solution.numSquares(4) == 1)
    println(Solution.numSquares(12) == 3)
    println(Solution.numSquares(13) == 2)
    println(Solution.numSquares(47))
    // println(Solution.minumSum(List(1, 4, 9), 12))
    // println(Solution.minumSum(List(1, 4, 9), 13))
  }

}
