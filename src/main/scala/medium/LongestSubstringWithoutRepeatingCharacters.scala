package medium

/** Longest Substring Without Repeating Characters
  * Given a string s, find the length of the longest substring without repeating characters.
  * Constraints:
  *
  * 0 <= s.length <= 5 * 10^4
  * s consists of English letters, digits, symbols and spaces.
  *
  * https://leetcode.com/problems/longest-substring-without-repeating-characters/
  */
trait LongestSubstringWithoutRepeatingCharacters {
  object Solution {
    def lengthOfLongestSubstring(s: String): Int = {
      var result = 0
      for {
        i <- s.indices
        longest = (i + 1 until s.length()).takeWhile { j =>
          !s.substring(i, j).contains(s(j))
        }
        subStr =
          if (longest.isEmpty) s(i).toString
          else s.substring(i, longest.last + 1)
      } yield {
        if (subStr.length() > result) {
          result = subStr.length()
        }
      }
      result
    }
  }

  def run() = {
    println(Solution.lengthOfLongestSubstring("abcabcbb") == 3)
    println(Solution.lengthOfLongestSubstring("bbbbb") == 1)
    println(Solution.lengthOfLongestSubstring("pwwkew") == 3)
    println(Solution.lengthOfLongestSubstring("") == 0)
  }
}
