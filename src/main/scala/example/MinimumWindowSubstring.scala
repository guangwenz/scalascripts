package example

/** Minimum Window Substring
  *
  * Given two strings s and t of lengths m and n respectively, return the minimum window substring of s such that every character in t (including duplicates) is included in the window. If there is no such substring, return the empty string "".
  *
  * The testcases will be generated such that the answer is unique.
  *
  * A substring is a contiguous sequence of characters within the string.
  * Constraints:
  *
  * m == s.length
  * n == t.length
  * 1 <= m, n <= 10^5
  * s and t consist of uppercase and lowercase English letters.
  *
  * Follow up: Could you find an algorithm that runs in O(m + n) time?
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/116/array-and-strings/838/
  */
trait MinimumWindowSubstring {
  object Solution {
    import collection.mutable.{Map => MMap}
    def minWindow(s: String, t: String): String = {
      if (s == t) s
      else if (s.length() < t.length()) ""
      else if (t.length() == 1 && s.indexOf(t) > -1) t
      else {
        val tMap = MMap.empty[Char, Int]
        t.foreach { c =>
          tMap.put(c, tMap.get(c).fold(1)(_ + 1))
        }
        def contains(l: Int, r: Int): Boolean = {
          // println(s"checking to see if ${s.substring(l, r)} contains $tMap")
          val sMap = MMap.empty[Char, Int]
          (l until r).foreach { i =>
            val ch = s(i)
            sMap.put(ch, sMap.get(ch).fold(1) { _ + 1 })
          }
          tMap.forall { case (ch, count) =>
            sMap.contains(ch) && sMap(ch) >= count
          }
        }
        var (l, r) = (0, s.length())
        var notFound = true
        while (l <= r && notFound) {
          if (contains(l + 1, r - 1)) {
            l += 1
            r -= 1
          } else if (contains(l + 1, r)) {
            l += 1
          } else if (contains(l, r - 1)) {
            r -= 1
          } else notFound = false
        }
        val ret =
          if (contains(l, r)) s.substring(l, r)
          else ""
        println(ret)
        ret
      }
    }
  }
  object Solution2 {
    import collection.mutable.{Map => MMap}
    def minWindow(s: String, t: String): String = {
      if (s == t) s
      else if (s.length() < t.length()) ""
      else if (t.length() == 1 && s.indexOf(t) > -1) t
      else {

        var result = Option.empty[String]
        val tCountMap = collection.mutable.Map.empty[Char, Int]
        t.foreach { ch =>
          tCountMap.get(ch) match {
            case Some(v) => tCountMap.put(ch, v + 1)
            case None    => tCountMap.put(ch, 1)
          }
        }

        val state = MMap.empty[Int, MMap[Char, Int]]

        for {
          i <- s.indices
          c = s(i)
          if tCountMap.contains(c)
        } yield {
          state.foreach { case (start, map) =>
            map.put(
              c,
              map.get(c).fold(1) { v =>
                math.min(v + 1, tCountMap(c))
              }
            )
            if (map == tCountMap) {
              val ans = s.substring(start, i + 1)
              result = result.fold(Some(ans)) { v =>
                if (v.length() > ans.length()) Some(ans) else Some(v)
              }
              state.remove(start)
            }
            if (i - start > result.map(_.length()).getOrElse(Int.MaxValue))
              state.remove(start)
          }

          state.put(i, MMap(c -> 1))
        }
        result.getOrElse("")
      }
    }
  }
  def run() = {
    println(Solution.minWindow("a", "a") == "a")
    println(Solution.minWindow("a", "aa") == "")
    println(Solution.minWindow("ab", "a") == "a")
    println(Solution.minWindow("a", "b") == "")
    println(Solution.minWindow("abc", "ac") == "abc")
    println(Solution.minWindow("acbbaca", "aba") == "baca")
    println(Solution.minWindow("aaaaaaaaaaaabbbbbcdd", "abcdd") == "abbbbbcdd")
    println(Solution.minWindow("ADOBECODEBANC", "ABC") == "BANC")
  }
}
