package easy

/** Valid Anagram
  *
  * Given two strings s and t, return true if t is an anagram of s, and false otherwise.
  *
  * Constraints:
  *
  * 1 <= s.length, t.length <= 5 * 10^4
  * s and t consist of lowercase English letters.
  *
  * https://leetcode.com/problems/valid-anagram/
  */
trait ValidAnagram {
  object Solution {
    def isAnagram(s: String, t: String): Boolean = {
      def freqMap(s: String): Map[Char, Int] =
        s.foldLeft(Map.empty[Char, Int]) { case (s, ch) =>
          s + (ch -> (s.getOrElse(ch, 0) + 1))
        }
      freqMap(s) == freqMap(t)
    }
  }
  def run() = {
    println(Solution.isAnagram("anagram", "nagaram") == true)
    println(Solution.isAnagram("rat", "car") == false)
  }
}
