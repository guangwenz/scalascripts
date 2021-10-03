package example

/** Container With Most Water
  * Given n non-negative integers a1, a2, ..., an , where each represents a point at coordinate (i, ai). n vertical lines are drawn such that the two endpoints of the line i is at (i, ai) and (i, 0). Find two lines, which, together with the x-axis forms a container, such that the container contains the most water.
  *
  * Notice that you may not slant the container.
  *
  * Constraints:
  *
  * n == height.length
  * 2 <= n <= 10^5
  * 0 <= height[i] <= 10^4
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/116/array-and-strings/830/
  */
trait ContainerWithMostWater {
  object Solution {
    def maxArea(height: Array[Int]): Int = {
      def areaFrom(i: Int, j: Int): Int = {
        (j - i) * (math.min(height(i), height(j)))
      }

      var start = 0
      var end = height.length - 1
      var ret = 0
      while (start < end) {
        var area = areaFrom(start, end)
        if (area > ret) ret = area
        if (height(start) < height(end)) start += 1
        else end -= 1
      }
      ret
    }
  }
  def run() = {
    println(Solution.maxArea(Array(1, 8, 6, 2, 5, 4, 8, 3, 7)) == 49)
    println(Solution.maxArea(Array(1, 1)) == 1)
    println(Solution.maxArea(Array(1, 2)) == 1)
    println(Solution.maxArea(Array(4, 3, 2, 1, 4)) == 16)
    println(Solution.maxArea(Array(1, 2, 1)) == 2)
    println(Solution.maxArea(Array(1, 2, 4, 3)) == 4)
    println(Solution.maxArea(Array(2, 3, 10, 5, 7, 8, 9)) == 36)
    println(Solution.maxArea(Array(2, 3, 4, 5, 18, 17, 6)) == 17)
    println(Solution.maxArea(Array(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)) == 25)
    println(Solution.maxArea(Array(4, 4, 2, 11, 0, 11, 5, 11, 13, 8)) == 55)
    println(
      Solution.maxArea(
        Array(75, 21, 3, 152, 13, 107, 163, 166, 32, 160, 41, 131, 7, 67, 56, 5,
          153, 176, 29, 139, 61, 149, 176, 142, 64, 75, 170, 156, 73, 48, 148,
          101, 70, 103, 53, 83, 11, 168, 1, 195, 81, 43, 126, 88, 62, 134, 45,
          167, 63, 26, 107, 124, 128, 83, 67, 192, 158, 189, 149, 184, 37, 49,
          85, 107, 152, 90, 143, 115, 58, 144, 62, 139, 139, 189, 180, 153, 75,
          177, 121, 138, 4, 28, 15, 132, 63, 82, 124, 174, 23, 25, 110, 60, 74,
          147, 120, 179, 37, 63, 94, 47)
      ) == 14608
    )
  }
}
