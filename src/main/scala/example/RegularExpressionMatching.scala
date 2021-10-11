package example

/** Regular Expression Matching
  * Given an input string s and a pattern p, implement regular expression matching with support for '.' and '*' where:
  *
  * '.' Matches any single character.​​​​
  * '*' Matches zero or more of the preceding element.
  * The matching should cover the entire input string (not partial).
  *
  * Constraints:
  *
  * 1 <= s.length <= 20
  * 1 <= p.length <= 30
  * s contains only lowercase English letters.
  * p contains only lowercase English letters, '.', and '*'.
  * It is guaranteed for each appearance of the character '*', there will be a previous valid character to match.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/119/backtracking/856/
  */
trait RegularExpressionMatching {
  object Solution {
    def isMatch(s: String, p: String): Boolean = {
      def inner(s: List[Char], p: List[Char]): Boolean = p match {
        case Nil               => s.isEmpty
        case '.' :: '*' :: Nil => true
        case a :: '*' :: tail =>
          inner(s, tail) || {
            val end = if (a == '.') s.length else s.takeWhile(_ == a).length
            (1 to end).exists(n => inner(s.drop(n), tail))
          }
        case '.' :: tail                            => s.nonEmpty && inner(s.tail, tail)
        case a :: tail if s.nonEmpty && a == s.head => inner(s.tail, tail)
        case _                                      => false
      }
      inner(s.toList, p.toList)
    }
  }

  def run() = {
    println(Solution.isMatch("aa", "a") == false)
    println(Solution.isMatch("a", ".*..a*") == false)
    println(Solution.isMatch("aa", "a*") == true)
    println(Solution.isMatch("ab", ".*") == true)
    println(Solution.isMatch("aab", "c*a*b") == true)
    println(Solution.isMatch("mississippi", "mis*is*p*.") == false)
    println(Solution.isMatch("bbbba", ".*a*a") == true)
  }
}
