package example

/** group anagrams
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/103/array-and-strings/778/
  */
trait GroupAnagrams {
  object Solution1 {
    def solve(strs: Array[String]): List[List[String]] = {
      val ret = strs.foldLeft(Map.empty[String, List[String]]) { case (s, i) =>
        val k = i.sorted
        val vs = s.get(k) match {
          case Some(value) => (i +: value)
          case None        => List(i)
        }
        s + (k -> vs)
      }
      ret.values.toList
    }
  }
  def run() = {
    val in1 = Array("eat", "tea", "tan", "ate", "nat", "bat")
    val in2 = Array("")
    val in3 = Array("bbbbb")
    println(Solution1.solve(in1))
  }
}
