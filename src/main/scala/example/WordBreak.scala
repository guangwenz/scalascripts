package example

/** Word Break
  * Given a string s and a dictionary of strings wordDict, return true if s can be segmented into a space-separated sequence of one or more dictionary words.
  *
  * Note that the same word in the dictionary may be reused multiple times in the segmentation.
  *
  * Constraints:
  *
  * 1 <= s.length <= 300
  * 1 <= wordDict.length <= 1000
  * 1 <= wordDict[i].length <= 20
  * s and wordDict[i] consist of only lowercase English letters.
  * All the strings of wordDict are unique.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/121/dynamic-programming/864/
  */
trait WordBreak {
  object Solution {
    def wordBreak(s: String, wordDict: List[String]): Boolean = {
      val memo = collection.mutable.Map.empty[String, Boolean]
      def inner(s: String): Boolean =
        if (memo.contains(s)) memo(s)
        else {
          val ret =
            if (s.isEmpty()) true
            else if (!wordDict.exists(i => s.startsWith(i))) false
            else {
              val p = for {
                w <- wordDict
                if s.startsWith(w) && inner(
                  s.substring(w.length())
                )
              } yield w
              p.nonEmpty
            }
          memo.put(s, ret)
          ret
        }
      inner(s)
    }
  }
  def run() = {
    println(
      Solution.wordBreak(
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",
        List(
          "a",
          "aa",
          "aaa",
          "aaaa",
          "aaaaa",
          "aaaaaa",
          "aaaaaaa",
          "aaaaaaaa",
          "aaaaaaaaa",
          "aaaaaaaaaa"
        )
      ) == false
    )
    println(Solution.wordBreak("leetcode", List("leet", "code")) == true)
    println(Solution.wordBreak("applepenapple", List("apple", "pen")) == true)
    println(
      Solution.wordBreak(
        "catsandog",
        List("cats", "dog", "sand", "and", "cat")
      ) == false
    )
  }
}
