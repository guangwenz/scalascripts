package easy

/**  Squares of a Sorted Array
  *
  * Given an integer array nums sorted in non-decreasing order, return an array of the squares of each number sorted in non-decreasing order.
  *
  * Constraints:
  *
  * 1 <= nums.length <= 10^4
  * -10^4 <= nums[i] <= 10^4
  * nums is sorted in non-decreasing order.
  */
trait SquaresofaSortedArray {
  object Solution {
    def sortedSquares(nums: Array[Int]): Array[Int] = {
      def merge(l: Array[Int], r: Array[Int]): Array[Int] = {
        @annotation.tailrec
        def inner(l: Array[Int], r: Array[Int], acc: Array[Int]): Array[Int] = {
          if (l.isEmpty) acc ++ r
          else if (r.isEmpty) acc ++ l
          else {
            if (l.head < r.head) inner(l.tail, r, acc :+ l.head)
            else inner(l, r.tail, acc :+ r.head)
          }
        }
        inner(l, r, Array.empty)
      }
      val (l, r) = nums.partition(_ < 0)
      merge(l.map(i => i * i).reverse, r.map(i => i * i))
    }
  }

  def run() = {
    println(
      Solution
        .sortedSquares(Array(-4, -1, 0, 3, 10))
        .sameElements(Array(0, 1, 9, 16, 100))
    )
    println(
      Solution
        .sortedSquares(Array(-7, -3, 2, 3, 11))
        .sameElements(Array(4, 9, 9, 49, 121))
    )
  }
}
