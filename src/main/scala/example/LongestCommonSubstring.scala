package example

/** find the longest common substring without repeating characters
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/103/array-and-strings/779/
  */
trait LongestCommonSubstring {
  object Solution1 {
    def solve(in: String): Int = in
      .foldLeft(Map.empty[String, Int]) { case (s, c) =>
        ???
      }
      .size
  }
  def run() = {
    val input = "abcabcbb"
    println(Solution1.solve(input))
  }
}
