package hard

/** Trapping Rain Water
  * Given n non-negative integers representing an elevation map where the width of each bar is 1, compute how much water it can trap after raining.
  *
  * Constraints:
  *
  * n == height.length
  * 1 <= n <= 2 * 10^4
  * 0 <= height[i] <= 10^5
  *
  * https://leetcode.com/problems/trapping-rain-water/
  */
trait TrappingRainWater {
  object Solution {
    def trap(height: Array[Int]): Int = {
        
      ???
    }
  }
  def run() = {
    println(Solution.trap(Array(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1)) == 6)
    println(Solution.trap(Array(4, 2, 0, 3, 2, 5)) == 9)
  }
}
