package medium

/** Permutation in String
  *
  * Given two strings s1 and s2, return true if s2 contains a permutation of s1, or false otherwise.
  *
  * In other words, return true if one of s1's permutations is the substring of s2.
  *
  * Constraints:
  *
  * 1 <= s1.length, s2.length <= 10^4
  * s1 and s2 consist of lowercase English letters.
  *
  * https://leetcode.com/problems/permutation-in-string/
  */
trait PermutationinString {
  object Solution {
    def matches(s: Array[Int], s2: Array[Int]): Boolean = {
      (0 until 26).forall(i => s(i) == s2(i))
    }
    def checkInclusion(s1: String, s2: String): Boolean = {
      val s1map = Array.fill(26)(0)
      (0 until s1.length).foreach { i =>
        s1map(s1(i) - 'a') += 1
      }
      for {
        i <- 0 to (s2.length() - s1.length())
        s2map = {
          val s2map = Array.fill(26)(0)
          (0 until s1.length()).foreach { j =>
            s2map(s2(i + j) - 'a') += 1
          }
          // println(s"s1 ${s1map.mkString(",")} and s2 ${s2map.mkString(",")}")
          s2map
        }
        _ = if (matches(s1map, s2map)) return true
      } yield ()
      false
    }
  }

  def run() = {
    println(Solution.checkInclusion("adc", "dcda") == true)
    println(Solution.checkInclusion("ab", "eidbaooo") == true)
    println(Solution.checkInclusion("ab", "eidboaoo") == false)
    println(Solution.checkInclusion("prosperity", "properties") == false)
    // println(
    //   Solution.checkInclusion(
    //     "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdef",
    //     "bcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefg"
    //   ) == false
    // )
  }
}
