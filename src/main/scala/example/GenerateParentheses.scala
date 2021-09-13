package example

/** Given n pairs of parentheses, write a function to generate all combinations of well-formed parentheses.
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/109/backtracking/794/
  */
trait GenerateParentheses {
  object Solution1 {
    def isValid(s: String): Boolean = s
      .foldLeft(List.empty[Char]) { case (s, i) =>
        val comp = if (i == '(') ')' else '('
        if (s.headOption.contains(comp)) s.tail else i +: s
      }
      .isEmpty

    def generateParenthesis(n: Int): List[String] = {
      if (n == 1) List("()")
      else {
        (for {
          p <- generateParenthesis(n - 1)
          x <- 0 to p.size
          (l, r) = p.splitAt(x)
          y <- 0 to r.size
          (l2, r2) = r.splitAt(y)
          str = l + "(" + l2 + ")" + r2
          if (isValid(str))
        } yield str).distinct
      }
    }
  }
  def run() = {
    println(Solution1.generateParenthesis(3).mkString("\n"))
  }
}
