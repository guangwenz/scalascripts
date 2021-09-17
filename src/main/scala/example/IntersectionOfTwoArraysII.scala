package example

/** Intersection of Two Arrays II
  * Given two integer arrays nums1 and nums2, return an array of their intersection. Each element in the result must appear as many times as it shows in both arrays and you may return the result in any order.
  * https://leetcode.com/explore/learn/card/binary-search/144/more-practices/1029/
  */
trait IntersectionOfTwoArraysII {
  object Solution {
    def binarySearch(target: Int, nums: Array[Int]): Option[Int] = {
      @annotation.tailrec
      def search(start: Int, end: Int): Option[Int] = {
        if (start > end) None
        else {
          val middle = (start + end) / 2
          if (nums(middle) == target) {
            Some((middle to 0 by -1).takeWhile(i => nums(i) == target).last)
          } else if (nums(middle) < target) search(middle + 1, end)
          else search(start, middle - 1)
        }
      }

      search(0, nums.size - 1)
    }

    def intersect(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
      @annotation.tailrec
      def loop(a: Array[Int], b: Array[Int], acc: Array[Int]): Array[Int] = {
        a match {
          case Array() => acc
          case Array(h, tail @ _*) =>
            binarySearch(h, b) match {
              case Some(v) =>
                val (l, r) = b.splitAt(v)
                loop(tail.toArray, r.tail, b(v) +: acc)
              case None => loop(tail.toArray, b, acc)
            }
        }
      }
      val (a, b) = (nums1.sorted, nums2.sorted)
      val (c, d) = if (a.size < b.size) (a, b) else (b, a)
      loop(c, d, Array.empty)
    }
  }

  def run() = {
    println(
      Solution.intersect(
        Array(4, 7, 9, 7, 6, 7),
        Array(5, 0, 0, 6, 1, 6, 2, 2, 4)
      ) sameElements (Array(4, 6))
    )
    println(
      Solution.intersect(
        Array(1, 2, 2, 1),
        Array(1, 1)
      ) sameElements (Array(1, 1))
    )
    println(
      Solution.intersect(
        Array(1, 2),
        Array(1, 1)
      ) sameElements (Array(1))
    )
    println(
      Solution.intersect(
        Array(4, 9, 5),
        Array(9, 4, 9, 8, 4)
      ) sameElements (Array(4, 9))
    )
  }
}
