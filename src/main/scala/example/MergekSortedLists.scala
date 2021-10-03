package example

/** Merge k Sorted Lists
  * You are given an array of k linked-lists lists, each linked-list is sorted in ascending order.
  *
  * Merge all the linked-lists into one sorted linked-list and return it.
  *
  * Constraints:
  *
  * k == lists.length
  * 0 <= k <= 10^4
  * 0 <= lists[i].length <= 500
  * -10^4 <= lists[i][j] <= 10^4
  * lists[i] is sorted in ascending order.
  * The sum of lists[i].length won't exceed 10^4.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/117/linked-list/839/
  */
trait MergekSortedLists {
  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  /** Definition for singly-linked list.
    * class ListNode(_x: Int = 0, _next: ListNode = null) {
    *   var next: ListNode = _next
    *   var x: Int = _x
    * }
    */
  object Solution {

    def find(target: ListNode)(fn: ListNode => Boolean): Option[ListNode] = {
      var current = target
      while (current != null) {
        if (fn(current)) return Some(current)
        current = current.next
      }
      None
    }
    def lastNode(head: ListNode): Option[ListNode] = {
      find(head)(_.next == null)
    }

    def findOverlap(
        source: ListNode,
        target: ListNode
    ): (ListNode, ListNode, ListNode) = {
      val lastTarget = lastNode(target).orNull
      val ret = if (source.x < target.x) {
        val l = source
        val o = find(source)(n =>
          n.x < target.x && (n.next == null || n.next.x >= target.x)
        ).map { n =>
          val t = n.next
          n.next = null
          t
        }
        val r = o.flatMap { s =>
          find(s) { n =>
            n.x <= lastTarget.x && (n.next != null && n.next.x > lastTarget.x)
          }.map { n =>
            val t = n.next
            n.next = null
            t
          }
        }
        (
          l,
          o.orNull,
          r.orNull
        )
      } else {
        (
          null,
          source,
          find(source) { n =>
            n.x <= lastTarget.x && (n.next != null && n.next.x > lastTarget.x)
          }.map { n =>
            val t = n.next
            n.next = null
            t
          }.orNull
        )
      }
      ret
    }

    def insert(node: ListNode, target: ListNode): ListNode = {
      if (node == null) return target
      val nodeNext = node.next
      find(target) { targetNode =>
        (targetNode.x <= node.x && (targetNode.next == null || targetNode.next.x > node.x)) || targetNode.next == null
      }.map { n =>
        val tmp = n.next
        n.next = node
        node.next = tmp
        insert(nodeNext, node)
      }
      target
    }

    def merge(source: ListNode, target: ListNode): ListNode = {
      if (source == null) return target
      if (target == null) return source
      val lastSource = lastNode(source).orNull
      val lastTarget = lastNode(target).orNull
      if (lastSource.x <= target.x) {
        lastSource.next = target
        source
      } else if (lastTarget.x <= source.x) {
        lastTarget.next = source
        target
      } else {
        val (sourceL, sourceOver, sourceR) = findOverlap(source, target)
        if (sourceL != null) {
          lastNode(sourceL).map { s =>
            s.next = target
          }
        }
        val newHead = if (sourceL == null) target else sourceL
        if (sourceOver != null)
          insert(sourceOver, newHead)
        lastNode(newHead).map(_.next = sourceR)
        if (sourceL != null) sourceL else target
      }
    }
    def mergeKLists(lists: Array[ListNode]): ListNode = {
      if (lists.isEmpty) null
      else
        lists.tail.foldLeft(lists.head) { case (s, i) =>
          merge(i, s)
        }
    }
  }

  def show(n: ListNode): String = {
    var current = n
    val vs = collection.mutable.ListBuffer.empty[Int]
    while (current != null) {
      vs += current.x
      current = current.next
    }
    vs.mkString(",")
  }
  def run() = {

    println(Solution.mergeKLists(Array.empty) == null)
    println(Solution.mergeKLists(Array(null)) == null)
    println(
      show(
        Solution.mergeKLists(
          Array(
            new ListNode(1, new ListNode(4, new ListNode(5))),
            new ListNode(1, new ListNode(3, new ListNode(4))),
            new ListNode(2, new ListNode(6))
          )
        )
      ) == "1,1,2,3,4,4,5,6"
    )
    println(
      show(
        Solution.mergeKLists(
          Array(
            new ListNode(1, new ListNode(4, new ListNode(5))),
            new ListNode(0, new ListNode(2))
          )
        )
      ) == "0,1,2,4,5"
    )
    println(
      show(
        Solution.mergeKLists(
          Array(
            new ListNode(1),
            new ListNode(0)
          )
        )
      ) == "0,1"
    )
    println(
      show(
        Solution.mergeKLists(
          Array(
            null,
            new ListNode(-2),
            new ListNode(-3, new ListNode(-2, new ListNode(1)))
          )
        )
      ) == "-3,-2,-2,1"
    )
    println(
      show(
        Solution.mergeKLists(
          Array(
            new ListNode(
              1,
              new ListNode(
                3,
                new ListNode(
                  4,
                  new ListNode(
                    6,
                    new ListNode(8, new ListNode(9, new ListNode(12)))
                  )
                )
              )
            ),
            new ListNode(
              1,
              new ListNode(
                2,
                new ListNode(
                  5,
                  new ListNode(
                    7,
                    new ListNode(11, new ListNode(21, new ListNode(24)))
                  )
                )
              )
            ),
            new ListNode(
              -4,
              new ListNode(
                0,
                new ListNode(
                  4,
                  new ListNode(
                    7,
                    new ListNode(
                      10,
                      new ListNode(14, new ListNode(22, new ListNode(29)))
                    )
                  )
                )
              )
            )
          )
        )
      ) == "-4,0,1,1,2,3,4,4,5,6,7,7,8,9,10,11,12,14,21,22,24,29"
    )
  }
}
