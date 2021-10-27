package easy

/** Valid Parentheses
  *
  * Given a string s containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid.
  *
  * An input string is valid if:
  *
  * Open brackets must be closed by the same type of brackets.
  * Open brackets must be closed in the correct order.
  *
  * Constraints:
  *
  * 1 <= s.length <= 10^4
  * s consists of parentheses only '()[]{}'.
  */
trait ValidParentheses {
  object Solution {
    def isValid(s: String): Boolean = {
      def matches(l: Char, r: Char): Boolean = {
        l match {
          case '(' => r == ')'
          case '[' => r == ']'
          case '{' => r == '}'
          case _   => false
        }
      }
      val invalid = s.foldLeft("") { case (s, i) =>
        i match {
          case ')' | ']' | '}' =>
            if (s.isEmpty()) return false
            else if (matches(s.head, i)) s.tail
            else return false
          case '(' | '[' | '{' => i +: s
        }
      }
      invalid.isEmpty()
    }
  }
  def run() = {
    println(Solution.isValid("()") == true)
    println(Solution.isValid("()[]{}") == true)
    println(Solution.isValid("(]") == false)
    println(Solution.isValid("([)]") == false)
    println(Solution.isValid("{[]}") == true)
  }
}
