package easy

/** Reverse Words in a String III
  *
  * Given a string s, reverse the order of characters in each word within a sentence while still preserving whitespace and initial word order.
  *
  * Constraints:
  *
  * 1 <= s.length <= 5 * 10^4
  * s contains printable ASCII characters.
  * s does not contain any leading or trailing spaces.
  * There is at least one word in s.
  * All the words in s are separated by a single space.
  *
  * https://leetcode.com/problems/reverse-words-in-a-string-iii/
  */
trait ReverseWordsinaStringIII {
  object Solution {
    def reverseWords(s: String): String = {
      def reverseWord(w: String): String = {
        var (l, r) = (0, w.length() - 1)
        val ar = w.toCharArray
        while (l < r) {
          val t = ar(l)
          ar(l) = ar(r)
          ar(r) = t
          l += 1
          r -= 1
        }
        ar.mkString
      }
      val p = for {
        w <- s.split(" ")
      } yield reverseWord(w)
      p.mkString(" ")
    }
  }

  def run() = {
    println(
      Solution.reverseWords(
        "Let's take LeetCode contest"
      ) == "s'teL ekat edoCteeL tsetnoc"
    )
    println(Solution.reverseWords("God Ding") == "doG gniD")
  }
}
