package example

/** find the longest common substring without repeating characters
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/103/array-and-strings/779/
  */
trait LongestCommonSubstring {
  object Solution {
    def lengthOfLongestSubstring(in: String): Int = {
      def getSub(i: Int): Int = {
        val acc = collection.mutable.Map(in(i) -> true)
        (i - 1 to 0 by -1).takeWhile { x =>
          val ret = acc.contains(in(x))
          if (!ret) acc.put(in(x), true)
          !ret
        }.length + 1
      }
      if (in.isEmpty()) 0
      else {
        val p = for {
          i <- 0 until in.length
          sub = getSub(i)
        } yield sub
        p.sorted(Ordering.Int.reverse).head
      }
    }
  }

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
    println(Solution.lengthOfLongestSubstring(" ") == 1)
    println(Solution.lengthOfLongestSubstring("abcabcbb") == 3)
    println(Solution.lengthOfLongestSubstring("bbbbbb") == 1)
    println(Solution.lengthOfLongestSubstring("pwwkew") == 3)
    println(Solution.lengthOfLongestSubstring("aabaab!bb") == 3)
  }
}
