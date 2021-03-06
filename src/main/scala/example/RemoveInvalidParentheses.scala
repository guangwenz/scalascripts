package example

/** Remove Invalid Parentheses
  * Given a string s that contains parentheses and letters, remove the minimum number of invalid parentheses to make the input string valid.
  *
  * Return all the possible results. You may return the answer in any order.
  *
  * Constraints:
  *
  * 1 <= s.length <= 25
  * s consists of lowercase English letters and parentheses '(' and ')'.
  * There will be at most 20 parentheses in s.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/119/backtracking/854/
  */

trait RemoveInvalidParentheses {
  object Solution {
    def removeInvalidParentheses(s: String): List[String] = {
      val invalidMemo =
        collection.mutable.Map.empty[(List[Char], List[Char]), List[String]]
      val pMemo = collection.mutable.Map.empty[List[Char], List[Char]]
      def invalidOnes(s: List[Char]): List[Char] = {
        if (pMemo.contains(s)) pMemo(s)
        else {
          val ret = s.foldLeft(List.empty[Char]) { case (s, c) =>
            c match {
              case '(' => c +: s
              case ')' => if (s.headOption.contains('(')) s.tail else c +: s
              case _   => s
            }
          }
          pMemo.put(s, ret)
          ret
        }
      }
      def removeInvalid(s: List[Char], invalid: List[Char]): List[String] = {
        if (invalidMemo.contains((s, invalid))) invalidMemo((s, invalid))
        else {
          val ret =
            if (s.isEmpty) List("")
            else
              invalid match {
                case Nil =>
                  if (invalidOnes(s).isEmpty) List(s.mkString)
                  else List.empty[String]
                case head :: tail =>
                  // println(
                  //   s"removing invalid from ${s.mkString} for invalid ${invalid.mkString}"
                  // )
                  val p = for {
                    i <- s.indices
                    c = s(i)
                    if c == head
                    (l, r) = s.splitAt(i)
                    ret = removeInvalid(l ++ r.tail, tail)
                    if ret.nonEmpty
                  } yield ret
                  p.toList.flatten.distinct
              }
          invalidMemo.put((s, invalid), ret)
          ret
        }
      }

      val in = s.toList
      removeInvalid(in, invalidOnes(in))
    }
  }
  object Solution2 {
    def removeInvalidParentheses(s: String): List[String] = {
      def getInvalidParentheses(s: String): String =
        s.foldLeft(List.empty[Char]) { case (s, ch) =>
          ch match {
            case '(' => ch +: s
            case ')' => if (s.headOption.contains('(')) s.tail else ch +: s
            case _   => s
          }
        }.mkString

      def remove(s: String, parentheses: String): List[String] = {
        def loop(
            s: String,
            parentheses: String
        ): List[String] = {
          if (parentheses.isEmpty()) List(s)
          else {
            val p = for {
              ci <- s.indices
              c = s(ci)
              if c == parentheses.head
            } yield {
              // val (l, r) = s.splitAt(ci)
              loop(
                new StringBuilder(s).deleteCharAt(ci).toString(),
                // l + r.tail,
                parentheses.tail
              )
            }
            p.flatten.toList
          }
        }
        val toCheck = loop(s, parentheses)
        // println(toCheck)
        toCheck.filter(getInvalidParentheses(_).isEmpty()).distinct
      }
      //find invalid parentheses not match
      //find ways to remove those parenthesis, each result will be part of the returning list
      remove(s, getInvalidParentheses(s))
    }
  }

  def run() = {
    println(
      Solution
        .removeInvalidParentheses("()())()")
        .equals(List("(())()", "()()()"))
    )
    println(
      Solution
        .removeInvalidParentheses("(a)())()")
        .sameElements(List("(a())()", "(a)()()"))
    )
    println(
      Solution
        .removeInvalidParentheses(")(")
        .equals(List(""))
    )
    println(
      Solution
        .removeInvalidParentheses("((()((s((((()")
        .equals(List("()s()", "()(s)", "(()s)"))
    )
  }
}
