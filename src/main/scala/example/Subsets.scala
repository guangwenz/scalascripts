package example

trait Subsets {
  object Solution {
    def combine(head: Int, l: List[Int]): List[List[Int]] = {
      List(l) ::: List(List(head)) ::: List(head +: l)
    }
    def subsets(nums: Array[Int]): List[List[Int]] = {
      nums.toList match {
        case Nil         => Nil
        case head :: Nil => List(List(), List(head))
        case head :: tail =>
          val p = for {
            sub <- subsets(tail.toArray)
            ret = combine(head, sub)
          } yield ret
          p.flatten.distinct
      }
    }
  }

  def run() = {
    println(Solution.subsets(Array(1)))
    println(Solution.subsets(Array(1, 2)))
    println(Solution.subsets(Array(1, 2, 3)))
  }
}
