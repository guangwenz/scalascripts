package hard

/** Maximize Palindrome Length From Subsequences
  * You are given two strings, word1 and word2. You want to construct a string in the following manner:
  *
  * Choose some non-empty subsequence subsequence1 from word1.
  * Choose some non-empty subsequence subsequence2 from word2.
  * Concatenate the subsequences: subsequence1 + subsequence2, to make the string.
  * Return the length of the longest palindrome that can be constructed in the described manner. If no palindromes can be constructed, return 0.
  *
  * A subsequence of a string s is a string that can be made by deleting some (possibly none) characters from s without changing the order of the remaining characters.
  *
  * A palindrome is a string that reads the same forward as well as backward.
  *
  * Constraints:
  *
  * 1 <= word1.length, word2.length <= 1000
  * word1 and word2 consist of lowercase English letters.
  *
  * https://leetcode.com/problems/maximize-palindrome-length-from-subsequences/
  */
trait MaximizePalindromeLengthFromSubsequences {
  object Solution {
    def longestPalindrome(word1: String, word2: String): Int = {
      val memo = collection.mutable.Map.empty[String, String]
      def longestPalindrome(word: String): String = {
        if (memo.contains(word)) memo(word)
        else {
          val dp = Array.fill(word.length() + 1, word.length() + 1)("")
          val wordR = word.reverse
          for {
            i <- 1 to word.length()
            j <- 1 to word.length()
          } yield {
            if (word(i - 1) == wordR(j - 1)) {
              dp(i)(j) = word(i - 1) + dp(i - 1)(j - 1)
            } else {
              dp(i)(j) =
                List(dp(i - 1)(j), dp(i)(j - 1)).sortBy(s => -s.length()).head
            }
          }
          val ret = dp(word.length())(word.length())
          // println(s"max palindrome for $word with result $ret")
          memo.put(word, ret)
          ret
        }
      }

      val word1Map = word1.zipWithIndex.toMap
      val word2Map = word2.zipWithIndex.toMap
      val word = word1 + word2
      var result = 0
      for {
        i <- word1.indices
        j <- word.length - 1 to word1.length() by -1
        sub = word.substring(i, j + 1)
        palin = longestPalindrome(sub)
        if word1Map.contains(palin.head) && word2Map.contains(palin.last)
      } yield {
        if (palin.size > result) {
          result = palin.size
          return result
        }
      }
      result
    }
  }
  object Solution2 {
    def longestPalindrome(word1: String, word2: String): Int = {
      @annotation.tailrec
      def isPalindrome(s: String): Boolean = {
        if (s.isEmpty()) true
        else {
          if (s.head != s.last) false
          else isPalindrome(s.tail.init)
        }
      }

      def subsequence(s: String): List[String] = {
        def subsequence(len: Int): List[String] = {
          def subFrom(start: Int, len: Int): List[String] = {
            len match {
              case x if x < 1 => Nil
              case 1          => s.substring(start).toList.map(_.toString())
              case _ =>
                for {
                  p <- (start until s.length()).toList
                  c = s(p)
                  sub <- subFrom(p + 1, len - 1)
                } yield {
                  c + sub
                }
            }
          }
          subFrom(0, len)
        }

        for {
          len <- (s.length() to 1 by -1).toList
          subseq <- subsequence(len)
        } yield subseq
      }

      var result = 0
      for {
        sub1 <- subsequence(word1)
        sub2 <- subsequence(word2)
        combined = sub1 + sub2
        if isPalindrome(combined)
      } yield {
        if (combined.length() > result) {
          println(s"sub1 is $sub1, sub2 is $sub2 = $combined")
          result = combined.length()
        }
      }
      result
    }
  }

  def run() = {
    println(Solution.longestPalindrome("cacb", "cbba") == 5)
    println(Solution.longestPalindrome("ab", "ab") == 3)
    println(Solution.longestPalindrome("aa", "bb") == 0)
    println(Solution.longestPalindrome("caaccfb", "decbaeaebedbfeea") == 15)
    println(Solution.longestPalindrome("ceebeddc", "d") == 3)
    println(
      Solution.longestPalindrome(
        "mucgtfgvtjsntplapadvusvtnwskkcungwqzptsvrqptvxsyotpfivqjsyzmtriij",
        "atybzoolhqogwpkwuemnbudlzaiyrxbjmakkjszbgwckdvuc"
      ) == 39
    )
  }
}
