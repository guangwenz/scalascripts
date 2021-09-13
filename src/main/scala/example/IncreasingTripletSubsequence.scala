package example

/** Given an integer array nums, return true if there exists a triple of indices (i, j, k) such that i < j < k and nums[i] < nums[j] < nums[k]. If no such indices exists, return false.
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/103/array-and-strings/781/
  */
trait IncreasingTripletSubsequence {

  /** naive solution
    * O(n^3)
    */
  object Solution1 {
    def solve(nums: Array[Int]): Boolean = {
      val p = for {
        i <- 0 until nums.size
        j <- i + 1 until nums.size
        k <- j + 1 until nums.size
        if (nums(i) < nums(j) && nums(j) < nums(k))
      } yield true
      p.nonEmpty
    }
  }

  object Solution2 {
    def solve(nums: Array[Int]): Boolean = {
      // def loop(in:Array[Int]) =
      ???
    }
  }
  def run() = {
    println(Solution2.solve(Array(1, 2, 3, 4, 5)) == true)
    println(Solution2.solve(Array(5, 4, 3, 2, 1)) == false)
    println(Solution2.solve(Array(2, 1, 5, 0, 4, 6)) == true)
    println(Solution2.solve(Array(20, 100, 10, 12, 5, 13)) == true)
  }
}
