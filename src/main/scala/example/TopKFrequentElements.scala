package example

/** Given an integer array nums and an integer k, return the k most frequent elements. You may return the answer in any order.
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/110/sorting-and-searching/799/
  */
trait TopKFrequentElements {
  object Solution {
    def topKFrequent(nums: Array[Int], k: Int): Array[Int] = {
      val m = nums.foldLeft(Map.empty[Int, Int]) { case (s, i) =>
        s.get(i) match {
          case Some(value) => s + (i -> (value + 1))
          case None        => s + (i -> 1)
        }
      }
      m.toArray.sortBy(-_._2).take(k).map(_._1)
    }
  }

  def run() = {
    println(Solution.topKFrequent(Array(1, 1, 1, 2, 2, 3), 2).mkString(","))
    println(Solution.topKFrequent(Array(1), 1).mkString(","))
  }
}
