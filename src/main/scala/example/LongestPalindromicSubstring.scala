package example

/** longest palindromic substring
  *
  * Given a string s, return the longest palindromic substring in s.
  *
  * Constraints:
  *
  * 1 <= s.length <= 1000
  * s consist of only digits and English letters.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/103/array-and-strings/780/
  */
trait LongestPalindromicSubstring {
  object Solution {
    def longestPalindrome(s: String): String = {
      def getPStr(left: Int, right: Int): String = {
        var (a, b) = (left - 1, right + 1)
        var pStr = ""
        while (a > -1 && b < s.length() && s(a) == s(b)) {
          pStr += s(a)
          a -= 1
          b += 1
        }
        pStr.reverse + s.substring(left, right + 1) + pStr
      }

      var lastPtr = s.head.toString()
      for {
        c <- 1 until s.length()
        pstr1 = if (s(c) == s(c - 1)) getPStr(c - 1, c) else ""
        pstr2 = getPStr(c, c)
        pstr = if (pstr1.length() > pstr2.length()) pstr1 else pstr2
        if pstr.length() > lastPtr.length()
      } yield {
        lastPtr = pstr
      }
      lastPtr
    }
  }

  /** Expand from one or 2 repeated pairs
    * O(n^2)
    */
  object Solution1 {
    def palindromStr(s: String, i: Int): String = {
      val pos = (i - 1 to 0 by -1) zip (i + 1 until s.size)
      val pairs = pos.takeWhile { case (l, r) =>
        s(l) == s(r)
      }
      pairs.map(i => s(i._1)).reverse.mkString + s(i).toString +
        pairs.map(i => s(i._2)).mkString
    }
    def palindromStr2(s: String, i: Int): String = {
      val pos = (i - 1 to 0 by -1) zip (i + 2 until s.size)
      val pairs = pos.takeWhile { case (l, r) =>
        s(l) == s(r)
      }
      pairs.map(i => s(i._1)).reverse.mkString + s(i).toString + s(
        i + 1
      ).toString +
        pairs.map(i => s(i._2)).mkString
    }

    def longestPalindrome(s: String): String = {
      val p = for {
        (c, i) <- s.zipWithIndex
        str = palindromStr(s, i)
      } yield str

      val p2 = for {
        (c, i) <- s.zipWithIndex if i < s.size - 1 && s(i) == s(i + 1)
        str = palindromStr2(s, i)
      } yield str
      val ret = p.sortBy(-_.size).headOption.getOrElse("")
      val ret2 = p2.sortBy(-_.size).headOption.getOrElse("")
      List(ret, ret2).sortBy(-_.size).headOption.getOrElse("")
    }
  }

  /** dynamic programming
    * Time: O(n^2)
    * Space: O(n^2)
    */
  object Solution2 {
    def longestPalindrome(s: String): String = {
      val dp = Array.fill(s.size, s.size)(false)
      (0 until s.size).foreach(i => dp(i)(i) = true)
      // for {}
      // println(dp.map(_.mkString(",")).mkString("\n"))
      ???
    }
  }

  /** brutal force
    * O(n^3)
    */
  object Solution3 {
    def isPalindrome(s: String): Boolean = s.reverse == s
    def longestPalindrome(s: String): String = {
      val p = for {
        m <- 0 to s.size
        n <- (m + 1 to s.size)
        sub = s.substring(m, n)
        if isPalindrome(sub)
      } yield sub
      p.sortBy(-_.size).headOption.getOrElse("")
    }
  }

  def run() = {
    println(Solution.longestPalindrome("babad") == "bab")
    println(Solution.longestPalindrome("cbbd") == "bb")
    println(Solution.longestPalindrome("a") == "a")
    println(Solution.longestPalindrome("ac") == "a")
    println(Solution.longestPalindrome("abb") == "bb")
    println(Solution.longestPalindrome("aaaabaaa") == "aaabaaa")
    println(Solution.longestPalindrome("aaba") == "aba")
    println(Solution.longestPalindrome("abcba") == "abcba")
  }
}
