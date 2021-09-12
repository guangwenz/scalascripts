package example

import scala.collection.immutable
import scala.annotation.tailrec

/** add 2 numbers from 2 linked list in reverse order
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/107/linked-list/783
  */
trait AddTwoNumbers {
  import AddTwoNumbers._

  object Solution1 {
    def solve(l1: ListNode, l2: ListNode): ListNode = {
      @tailrec
      def loop(
          l1: Option[ListNode],
          l2: Option[ListNode],
          last: Option[ListNode],
          answer: Option[ListNode]
      ): Option[ListNode] = {
        def newLast(v: Int): ListNode = last.fold(new ListNode(v)) { l =>
          val inc = if (l.x > 9) {
            l.x = l.x % 10
            1
          } else 0
          l.next = new ListNode(v + inc)
          l.next
        }

        (l1, l2) match {
          case (Some(v1), Some(v2)) =>
            val n = newLast(v1.x + v2.x)
            loop(
              Option(v1.next),
              Option(v2.next),
              Some(n),
              answer.orElse(Some(n))
            )
          case (Some(v1), None) =>
            val n = newLast(v1.x)
            loop(
              Option(v1.next),
              None,
              Some(n),
              answer.orElse(Some(n))
            )
          case (None, Some(v1)) =>
            val n = newLast(v1.x)
            loop(
              None,
              Option(v1.next),
              Some(n),
              answer.orElse(Some(n))
            )
          case (None, None) =>
            last.map { a =>
              if (a.x > 9) {
                a.x = a.x % 10
                a.next = new ListNode(1)
                a.next
              } else a
            }
            answer
        }
      }

      loop(Option(l1), Option(l2), None, None).getOrElse(null)
    }
  }
  def createNode(in: List[Int]): ListNode = {
    val root = new ListNode(1, null)
    in.foldLeft(root) { case (s, i) =>
      s.next = new ListNode(i)
      s.next
    }
    root.next
  }
  def run() = {
    val l1 = createNode(
      List(2, 4, 3)
    ) //new ListNode(2, new ListNode(4, new ListNode(3)))
    val l2 = createNode(
      List(5, 6, 4)
    ) //new ListNode(5, new ListNode(6, new ListNode(4)))

    var ret = Solution1.solve(
      createNode((1 to 7).map(_ => 9).toList), //List(9, 9, 9, 9, 9, 9, 9)),
      createNode((1 to 4).map(_ => 9).toList) //List(9, 9, 9, 9))
    )
    while (ret != null) {
      println(ret.x)
      ret = ret.next
    }
  }
}

object AddTwoNumbers {
  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }
}
