package easy

/** Ransom Note
  *
  * Given two stings ransomNote and magazine, return true if ransomNote can be constructed from magazine and false otherwise.
  *
  * Each letter in magazine can only be used once in ransomNote.
  *
  * Constraints:
  *
  * 1 <= ransomNote.length, magazine.length <= 10^5
  * ransomNote and magazine consist of lowercase English letters.
  *
  * https://leetcode.com/problems/ransom-note/
  */
trait RansomNote {
  object Solution {
    def canConstruct(ransomNote: String, magazine: String): Boolean = {
      if (ransomNote.isEmpty()) true
      else {
        magazine.indexOf(ransomNote.head) match {
          case -1 => return false
          case v =>
            canConstruct(
              ransomNote.tail,
              magazine.replaceFirst(ransomNote.head.toString(), "")
            )
        }
      }
    }
  }

  def run() = {
    println(Solution.canConstruct("a", "b") == false)
    println(Solution.canConstruct("aa", "ab") == false)
    println(Solution.canConstruct("aa", "aab") == true)
  }
}
