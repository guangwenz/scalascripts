package easy

/** First Unique Character in a String
  *
  * Given a string s, find the first non-repeating character in it and return its index. If it does not exist, return -1.
  *
  * Constraints:
  *
  * 1 <= s.length <= 10^5
  * s consists of only lowercase English letters.
  *
  * https://leetcode.com/problems/first-unique-character-in-a-string/
  */
trait FirstUniqueCharacterinaString {
  object Solution {
    def firstUniqChar(s: String): Int = {
      val freqMap = collection.mutable.Map.empty[Char, Int]
      val startMap = collection.mutable.Map.empty[Char, Int]
      for {
        i <- 0 until s.length()
        ch = s(i)
      } yield {
        freqMap.put(ch, freqMap.getOrElse(ch, 0) + 1)
        if (!startMap.contains(ch)) startMap.put(ch, i)
      }
      val unique = freqMap.filter(_._2 == 1)
      var result = Int.MaxValue
      for {
        (k, v) <- startMap
        if unique.contains(k)
      } yield {
        result = math.min(result, v)
      }
      if (result == Int.MaxValue) -1 else result
    }
  }
  def run() = {
    println(Solution.firstUniqChar("leetcode") == 0)
    println(Solution.firstUniqChar("loveleetcode") == 2)
    println(Solution.firstUniqChar("aabb") == -1)
    println(Solution.firstUniqChar("aadadaad") == -1)
  }
}
