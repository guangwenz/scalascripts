package example

/** longest palindromic substring
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/103/array-and-strings/780/
  */
trait LongestPalindromicSubstring {

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
    println(Solution2.longestPalindrome("babad") == "bab")
    println(Solution2.longestPalindrome("cbbd") == "bb")
    println(Solution2.longestPalindrome("a") == "a")
    println(Solution2.longestPalindrome("ac") == "a")
    println(Solution2.longestPalindrome("abb") == "bb")
    println(Solution2.longestPalindrome("aaaabaaa") == "aaabaaa")
    println(Solution2.longestPalindrome("aaba") == "aba")
    println(Solution2.longestPalindrome("abcba") == "abcba")
  }
}
