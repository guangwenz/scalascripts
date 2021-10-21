package easy

/** Reverse String
  * Write a function that reverses a string. The input string is given as an array of characters s.
  *
  * Constraints:
  *
  * 1 <= s.length <= 10^5
  * s[i] is a printable ascii character.
  *
  * https://leetcode.com/problems/reverse-string/
  */
trait ReverseString {
  object Solution {
    def reverseString(s: Array[Char]): Unit = {
      var (l, r) = (0, s.length - 1)
      while (l < r) {
        val t = s(l)
        s(l) = s(r)
        s(r) = t
        l += 1
        r -= 1
      }
    }
  }
  def run() = {
    val input1 = Array('h', 'e', 'l', 'l', 'o')
    Solution.reverseString(input1)
    println(input1.sameElements(Array('o', 'l', 'l', 'e', 'h')))
    val input2 = Array('H', 'a', 'n', 'n', 'a', 'h')
    Solution.reverseString(input2)
    println(input2.sameElements(Array('h', 'a', 'n', 'n', 'a', 'H')))
  }
}
