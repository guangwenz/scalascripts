package example

/** Wildcard Matching
  * Given an input string (s) and a pattern (p), implement wildcard pattern matching with support for '?' and '*' where:
  *
  * '?' Matches any single character.
  * '*' Matches any sequence of characters (including the empty sequence).
  * The matching should cover the entire input string (not partial).
  *
  * Constraints:
  *
  * 0 <= s.length, p.length <= 2000
  * s contains only lowercase English letters.
  * p contains only lowercase English letters, '?' or '*'.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/119/backtracking/855/
  */
trait WildcardMatching {

  /** recursive solution with out time out at 1710 / 1811 test cases
    */
  object Solution {
    def isMatch(s: String, p: String): Boolean = {
      if (p == "*") true
      else if (!p.split("[\\*\\?]").forall(s.contains)) false
      else if (p.isEmpty()) s.isEmpty()
      else if (s.isEmpty()) p.distinct == "*"
      else {
        p match {
          case x if x.startsWith("*") =>
            (s.length to 0 by -1).exists(n =>
              isMatch(s.drop(n), x.dropWhile(_ == '*'))
            )
          case x if x.startsWith("?") => isMatch(s.tail, x.tail)
          case x if x.head == s.head  => isMatch(s.tail, x.tail)
          case _                      => false
        }
      }
    }
  }

  def run() = {
    // println(Solution.isMatch("aa", "a") == false)
    // println(Solution.isMatch("aa", "*") == true)
    // println(Solution.isMatch("cb", "?a") == false)
    // println(Solution.isMatch("adceb", "*a*b") == true)
    // println(Solution.isMatch("acdcb", "a*c?b") == false)
    println(
      Solution.isMatch(
        "abbabaaabbabbaababbabbbbbabbbabbbabaaaaababababbbabababaabbababaabbbbbbaaaabababbbaabbbbaabbbbababababbaabbaababaabbbababababbbbaaabbbbbabaaaabbababbbbaababaabbababbbbbababbbabaaaaaaaabbbbbaabaaababaaaabb",
        "**aa*****ba*a*bb**aa*ab****a*aaaaaa***a*aaaa**bbabb*b*b**aaaaaaaaa*a********ba*bbb***a*ba*bb*bb**a*b*bb"
      ) == false
    )
    // println(
    //   Solution.isMatch(
    //     "aaabbbaabaaaaababaabaaabbabbbbbbbbaabababbabbbaaaaba",
    //     "a*******b"
    //   ) == false
    // )
    println(
      Solution.isMatch(
        "abbabbbaabaaabbbbbabbabbabbbabbaaabbbababbabaaabbab",
        "*aabb***aa**a******aa*"
      ) == true
    )
  }
}
