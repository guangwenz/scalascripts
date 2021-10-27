package medium

/** Letter Case Permutation
  *
  * Given a string s, we can transform every letter individually to be lowercase or uppercase to create another string.
  *
  * Return a list of all possible strings we could create. You can return the output in any order.
  *
  * Constraints:
  *
  * s will be a string with length between 1 and 12.
  * s will consist only of letters or digits.
  *
  * https://leetcode.com/problems/letter-case-permutation/
  */
trait LetterCasePermutation {
  object Solution {
    def letterCasePermutation(s: String): List[String] = {
      if (s.length() == 1) {
        if (s.head.isDigit) List(s)
        else List(s.toLowerCase(), s.toUpperCase())
      } else {
        val sub = letterCasePermutation(s.tail)
        if (s.head.isDigit) sub.map(s.head +: _)
        else {
          sub.map(s.head.toUpper +: _) ::: sub.map(s.head.toLower +: _)
        }
      }
    }
  }

  def run() = {
    println(
      Solution
        .letterCasePermutation("a1b2")
      // .sameElements(List("a1b2", "a1B2", "A1b2", "A1B2"))
    )
    println(
      Solution
        .letterCasePermutation("3z4")
        .sameElements(List("3z4", "3Z4"))
    )
    println(
      Solution
        .letterCasePermutation("12345")
        .sameElements(List("12345"))
    )
    println(
      Solution
        .letterCasePermutation("0")
        .sameElements(List("0"))
    )
    println(
      Solution
        .letterCasePermutation("c")
      // .sameElements(List("c", "C"))
    )
  }
}
