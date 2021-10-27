package medium

/** Find All Anagrams in a String
  * Given two strings s and p, return an array of all the start indices of p's anagrams in s. You may return the answer in any order.
  *
  * An Anagram is a word or phrase formed by rearranging the letters of a different word or phrase, typically using all the original letters exactly once.
  *
  * Constraints:
  *
  * 1 <= s.length, p.length <= 3 * 10^4
  * s and p consist of lowercase English letters.
  *
  * https://leetcode.com/problems/find-all-anagrams-in-a-string/
  */
trait FindAllAnagramsinaString {
  object Solution {
    def findAnagrams(s: String, p: String): List[Int] = {
      if (p.size > s.size) Nil
      else {
        val pMap = collection.mutable.Map.empty[Char, Int]
        p.foreach { c => pMap.put(c, pMap.getOrElse(c, 0) + 1) }
        val map = collection.mutable.Map.empty[Char, Int]
        (0 until p.size).foreach { i =>
          map.put(s(i), map.getOrElse(s(i), 0) + 1)
        }
        val init =
          if (map == pMap) List(0)
          else List.empty[Int]
        val prg = (p.length until s.length())
          .foldLeft((0, init)) { case ((start, acc), j) =>
            val ch = s(start)
            map.get(ch) match {
              case Some(v) if v == 1 => map.remove(ch)
              case Some(v)           => map.put(ch, v - 1)
              case None              => map
            }
            val last = s(j)
            map.put(last, map.getOrElse(last, 0) + 1)
            // println(map)
            if (map == pMap) (start + 1, acc :+ start + 1)
            else (start + 1, acc)
          }
        // println(prg)
        prg._2
      }
    }
  }
  def run() {
    println(Solution.findAnagrams("cbaebabacd", "abc").sameElements(List(0, 6)))
    println(Solution.findAnagrams("abab", "ab").sameElements(List(0, 1, 2)))
  }
}
