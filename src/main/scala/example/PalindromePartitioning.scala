package example

/** Palindrome Partitioning
  * Given a string s, partition s such that every substring of the partition is a palindrome. Return all possible palindrome partitioning of s.
  *
  * A palindrome string is a string that reads the same backward as forward.
  *
  * Constraints:
  *
  * 1 <= s.length <= 16
  * s contains only lowercase English letters.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/119/backtracking/852/
  */
trait PalindromePartitioning {

  /** some efficient solution from internet
    */
  object Solution2 {
    def partition(s: String): List[List[String]] = {
      var length = s.length;
      var partitions = scala.collection.mutable.ListBuffer[List[String]]()
      var a = Array.ofDim[Boolean](length, length); var i = 0;

      for (i <- 0 until length) a(i)(i) = true;
      for (i <- 0 until length - 1) a(i)(i + 1) = s(i) == s(i + 1);
      for (j <- 2 until length)
        for (i <- 0 until length - j)
          a(i)(i + j) = a(i + 1)(i + j - 1) && s(i) == s(i + j);

      def helper(index: Int, p: List[String]): Unit = {
        if (index == length) partitions += p;
        else {
          var k = 0;
          for (k <- index until length)
            if (a(index)(k))
              helper(k + 1, p :+ s.slice(index, k + 1))
        }
      }
      helper(0, List());
      partitions.toList
    }
  }

  object Solution {
    def isPalindrome(s: String): Boolean =
      s.length() match {
        case 0 => true
        case 1 => true
        case 2 => s(0) == s(1)
        case _ =>
          s.length % 2 match {
            case 0 =>
              val (a1, a2) = (s.length / 2, s.length / 2 - 1)
              s(a1) == s(a2) && (a2 + 1 until s.length).forall { gap =>
                val len = s.length - gap
                s(a1 - len) == s(a2 + len)
              }
            case 1 =>
              val m = s.length / 2
              (m + 1 until s.length).forall { gap =>
                val len = s.length - gap
                s(m - len) == s(m + len)
              }
          }
      }
    def partition(s: String): List[List[String]] = {
      val memo = collection.mutable.Map.empty[String, List[List[String]]]
      def inner(s: String): List[List[String]] = {
        if (memo.contains(s)) memo(s)
        else {
          val ret =
            if (s.size == 1) List(List(s.head.toString()))
            else
              for {
                i <- List.range(1, s.length + 1)
                sub = s.substring(0, i)
                if isPalindrome(sub)
                rest = s.substring(i, s.length())
                ans <- {
                  if (rest.isEmpty()) List(List(sub))
                  else partition(rest).map(sub +: _)
                }
              } yield ans
          memo.put(s, ret)
          ret
        }
      }
      inner(s)
    }
  }

  def run() = {
    // println(Solution.isPalindrome("") == true)
    // println(Solution.isPalindrome("a") == true)
    // println(Solution.isPalindrome("aa") == true)
    // println(Solution.isPalindrome("aab") == false)
    // println(Solution.isPalindrome("baab") == true)
    // println(Solution.isPalindrome("abaabc") == false)
    // println(Solution.isPalindrome("cbaabc") == true)
    println(
      Solution
        .partition("aab")
        .sameElements(List(List("a", "a", "b"), List("aa", "b")))
    )

    println(
      Solution
        .partition("a")
        .sameElements(List(List("a")))
    )
    println(
      Solution
        .partition("bb")
        .sameElements(List(List("b", "b"), List("bb")))
    )
  }
}
