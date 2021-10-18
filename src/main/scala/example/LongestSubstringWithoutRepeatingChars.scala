package example

/** Given a string, find the length of the longest substring without repeating characters.
  *
  * Examples:
  *
  * Given "abcabcbb", the answer is "abc", which the length is 3.
  *
  * Given "bbbbb", the answer is "b", with the length of 1.
  *
  * Given "pwwkew", the answer is "wke", with the length of 3. Note that the answer must be a substring, "pwke" is a subsequence and not a substring.
  */

trait LongestSubstringWithoutRepeatingChars {
  object Solution {
    def longest(in: String): Int = {
      var result = ""
      val p = for {
        i <- in.indices
        ends = (i + 1 until in.length).takeWhile { j =>
          val ch = in(j)
          val prefix = in.substring(i, j)
          !prefix.contains(ch)
        }
        word =
          if (ends.isEmpty) in(i).toString
          else in.substring(i, math.min(ends.end + 1, in.length - 1))
      } yield {
        if (word.length > result.length()) result = word
      }
      result.length()
    }
  }

  def run() = {
    println(Solution.longest("abcabcbb") == 3)
    println(Solution.longest("bbbbb") == 1)
    println(Solution.longest("pwwkew") == 3)
  }
}
