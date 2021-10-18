package medium

/** Longest Palindromic Subsequence
  * Given a string s, find the longest palindromic subsequence's length in s.
  *
  * A subsequence is a sequence that can be derived from another sequence by deleting some or no elements without changing the order of the remaining elements.
  *
  * Constraints:
  *
  * 1 <= s.length <= 1000
  * s consists only of lowercase English letters.
  *
  * https://leetcode.com/problems/longest-palindromic-subsequence/
  */
trait LongestPalindromicSubsequence {
  object Solution {
    def longestPalindromeSubseq(s: String): Int = {
      val dp = Array.fill(s.length() + 1, s.length() + 1)(0)
      val r = s.reverse
      for {
        i <- 1 to s.length
        j <- 1 to r.length
      } yield {
        if (s(i - 1) == r(j - 1)) {
          dp(i)(j) = 1 + dp(i - 1)(j - 1)
        } else {
          dp(i)(j) = math.max(dp(i - 1)(j), dp(i)(j - 1))
        }
      }
      dp(s.length)(s.length())
    }
  }

  def run() = {
    println(Solution.longestPalindromeSubseq("bbbab") == 4)
    println(Solution.longestPalindromeSubseq("cbbd") == 2)
  }
}
