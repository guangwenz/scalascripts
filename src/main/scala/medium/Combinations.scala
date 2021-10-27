package medium

/** Combinations
  * Given two integers n and k, return all possible combinations of k numbers out of the range [1, n].
  *
  * You may return the answer in any order.
  * Constraints:
  *
  * 1 <= n <= 20
  * 1 <= k <= n
  */
trait Combinations {
  object Solution {
    def combine(n: Int, k: Int): List[List[Int]] = {
      val nums = List.range(1, n + 1)
      def inner(start: Int, k: Int): List[List[Int]] = {
        if (k == 1) List.range(start, nums.length).map(i => List(nums(i)))
        else {
          for {
            i <- List.range(start, nums.length)
            sub <- inner(i + 1, k - 1)
          } yield nums(i) +: sub
        }
      }
      inner(0, k)
    }
  }
  def run() = {
    println(Solution.combine(4, 2))
    println(Solution.combine(1, 1))
  }
}
