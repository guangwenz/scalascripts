package example

import scala.util.Random
import scala.annotation.tailrec

/** common sorting algo
  */
trait Sorting {

object HeapSort{
  def sort(in: Array[Int]): Array[Int] = ???
}
  /** quick sort with tail recursion
    * divide and conquer
    * pick a pivot, partion the list into 2, sort each and combine them
    */
  object QuickSort {
    def sort(in: List[Int]): List[Int] = {
      in match {
        case Nil         => Nil
        case head :: Nil => in
        case head :: tail =>
          val p = in(in.size / 2)
          val (l, r) = in.partition(_ < p)
          sort(l) ::: List(p) ::: sort(r.filterNot(_ == p))
      }
    }
  }

  /** merge sort with tail recursion
    * divide and conquer algo
    * divide the list into 2 and sort them, then merge them
    */
  object MergeSort {
    @annotation.tailrec
    def merge(l: List[Int], r: List[Int], acc: List[Int]): List[Int] =
      (l, r) match {
        case (Nil, rs) => acc ::: rs
        case (ls, Nil) => acc ::: ls
        case (lh :: lt, rh :: rt) =>
          if (lh < rh) merge(lt, r, acc :+ lh)
          else merge(l, rt, acc :+ rh)
      }

    def sort(in: List[Int]): List[Int] = {
      in match {
        case Nil         => Nil
        case head :: Nil => in
        case _ =>
          val middle = in.size / 2
          val (left, right) = in.splitAt(middle)
          merge(sort(left), sort(right), Nil)
      }
    }
  }

  /** insertion sort with tail recursion
    */
  object InsertionSort {
    @tailrec
    def sort(in: List[Int], acc: List[Int]): List[Int] = {
      in match {
        case Nil          => acc
        case head :: tail => sort(tail, insert(head, acc, Nil))
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
    // show(QuickSort.sort(in))
    // show(InsertionSort.sort(in, List.empty))
    show(MergeSort.sort(in))
    // show((0 to 100000).map(_ => Random.nextInt).toList.sorted)
  }
}
