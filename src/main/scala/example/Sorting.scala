package example

import scala.util.Random
import scala.annotation.tailrec

/** common sorting algo
  */
trait Sorting {

  /** quick sort with tail recursion
    * divide and conquer
    * pick a pivot, partion the list into 2, sort each and combine them
    */
  object QuickSort {
    def solve(in: List[Int]): List[Int] = {
      in match {
        case Nil         => Nil
        case head :: Nil => in
        case head :: tail =>
          val p = in(in.size / 2)
          val (l, r) = in.partition(_ < p)
          solve(l) ::: List(p) ::: solve(r.filterNot(_ == p))
      }
    }
  }

  /** merge sort with tail recursion
    * divide and conquer algo
    * divide the list into 2 and sort them, then merge them
    */
  object MergeSort {
    @tailrec
    def merge(l: List[Int], r: List[Int], acc: List[Int]): List[Int] =
      (l, r) match {
        case (Nil, rs) => rs ::: acc
        case (ls, Nil) => ls ::: acc
        case (lh :: lt, rh :: rt) =>
          if (lh < rh) merge(lt, r, lh :: acc)
          else merge(l, rt, rh :: acc)
      }

    def solve(in: List[Int]): List[Int] = {
      val middle = in.size / 2

      if (middle == 0) in
      else {
        val (left, right) = in.splitAt(middle)
        merge(solve(left), solve(right), List.empty)
      }
    }
  }

  /** insertion sort with tail recursion
    */
  object InsertionSort {
    @tailrec
    def solve(in: List[Int], acc: List[Int]): List[Int] = {
      in match {
        case Nil          => acc
        case head :: tail => solve(tail, insert(head, acc, Nil))
      }
    }

    @tailrec
    def insert(item: Int, sorted: List[Int], acc: List[Int]): List[Int] =
      sorted match {
        case Nil => acc.reverse ++ List(item)
        case head :: tail =>
          if (item <= head) acc.reverse ++ (item :: sorted)
          else {
            insert(item, tail, head :: acc)
          }
      }
  }

  def show(in: List[Int]): Unit = println(in.mkString("\n"))
  def run() = {
    val in = (0 to 1000000).map(_ => Random.nextInt()).toList
    // show(QuickSort.solve(in))
    // show(InsertionSort.solve(in, List.empty))
    show(MergeSort.solve(in))
    // show((0 to 100000).map(_ => Random.nextInt).toList.sorted)
  }
}
