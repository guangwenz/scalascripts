package example

/** find the longest common substring without repeating characters
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/103/array-and-strings/779/
  */
trait LongestCommonSubstring {
  object Solution1 {
    def solve(in: String): Int = {
      @annotation.tailrec
      def loop(
          data: List[Char],
          current: List[Char],
          solution: List[Char]
      ): List[Char] =
        data match {
          case Nil => if (current.size > solution.size) current else solution
          case head :: tail =>
            if (current.contains(head)) {
              val newCurrent =
                current.splitAt(current.lastIndexOf(head))._2.tail ::: List(
                  head
                )
              val newSolution =
                if (current.size >= solution.size) current
                else solution
              loop(tail, newCurrent, newSolution)
            } else
              loop(tail, current ::: List(head), solution)
        }

      loop(in.toList, Nil, Nil).size
    }
  }
  def run() = {
    val input = "abcabcbb"
    println(Solution1.solve(" ") == 1)
    println(Solution1.solve("abcabcbb") == 3)
    println(Solution1.solve("bbbbbb") == 1)
    println(Solution1.solve("pwwkew") == 3)

    println(Solution1.solve("aabaab!bb") == 3)
  }
}
