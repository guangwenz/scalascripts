package medium

/** Decode Ways
  * A message containing letters from A-Z can be encoded into numbers using the following mapping:
  *
  * 'A' -> "1"
  * 'B' -> "2"
  * ...
  * 'Z' -> "26"
  * To decode an encoded message, all the digits must be grouped then mapped back into letters using the reverse of the mapping above (there may be multiple ways). For example, "11106" can be mapped into:
  *
  * "AAJF" with the grouping (1 1 10 6)
  * "KJF" with the grouping (11 10 6)
  * Note that the grouping (1 11 06) is invalid because "06" cannot be mapped into 'F' since "6" is different from "06".
  *
  * Given a string s containing only digits, return the number of ways to decode it.
  *
  * The answer is guaranteed to fit in a 32-bit integer.
  * Constraints:
  *
  * 1 <= s.length <= 100
  * s contains only digits and may contain leading zero(s).
  *
  * https://leetcode.com/problems/decode-ways/
  */
trait DecodeWays {
  object Solution {
    def numDecodings(s: String): Int = {
      if (s.startsWith("0")) 0
      else {
        val dp = Array.fill(s.length())(1)
        for {
          i <- 1 until s.length()
          prevI = s(i - 1).toString.toInt
          chI = s(i).toString.toInt
          twoDigit = prevI * 10 + chI
        } yield {
          if (twoDigit == 0) return 0
          if (chI == 0 && prevI > 2) return 0
          if (twoDigit == 10 || twoDigit == 20) {
            dp(i) = if (i - 2 >= 0) dp(i - 2) else 1
          } else if (twoDigit < 27 && twoDigit > 10) dp(i) = {
            val v = if (i - 2 >= 0) dp(i - 2) else 1
            v + dp(i - 1)
          }
          else dp(i) = dp(i - 1)
        }
        dp.last
      }
    }
  }

  def run() = {
    println(Solution.numDecodings("12") == 2)
    println(Solution.numDecodings("226") == 3)
    println(Solution.numDecodings("6") == 1)
    println(Solution.numDecodings("0") == 0)
    println(Solution.numDecodings("06") == 0)
    println(Solution.numDecodings("10") == 1)
    println(Solution.numDecodings("2101") == 1)
    println(Solution.numDecodings("1123") == 5)
    println(Solution.numDecodings("10011") == 0)
  }
}
