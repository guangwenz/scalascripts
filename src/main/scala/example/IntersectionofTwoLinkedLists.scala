package example

/** Intersection of Two Linked Lists
  *
  * Given the heads of two singly linked-lists headA and headB, return the node at which the two lists intersect. If the two linked lists have no intersection at all, return null.
  *
  * Constraints:
  *
  *  The number of nodes of listA is in the m.
  *  The number of nodes of listB is in the n.
  *  0 <= m, n <= 3 * 104
  *  1 <= Node.val <= 105
  *  0 <= skipA <= m
  *  0 <= skipB <= n
  *  intersectVal is 0 if listA and listB do not intersect.
  *  intersectVal == listA[skipA] == listB[skipB] if listA and listB intersect.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/107/linked-list/785/
  */
trait IntersectionofTwoLinkedLists {
  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
  }

  object ListNode {
    def apply(v: Int, next: ListNode): ListNode = {
      val n = new ListNode(v)
      n.next = next
      n
    }
  }

  /** Definition for singly-linked list.
    * class ListNode(var _x: Int = 0) {
    *   var next: ListNode = null
    *   var x: Int = _x
    * }
    */

  object Solution {
    def find(list: ListNode, target: ListNode): Option[ListNode] = {
      if (list == null) None
      else {
        if (list == target) Some(target)
        else find(list.next, target)
      }
    }
    def getIntersectionNode(headA: ListNode, headB: ListNode): ListNode = {
      if (headA == null) null
      else {
        find(headB, headA) match {
          case None =>
            getIntersectionNode(headA.next, headB)
          case Some(v) => v
        }
      }
    }
  }

  def run() = {
    val sharedN = ListNode(8, ListNode(4, ListNode(5, null)))
    println(
      Solution
        .getIntersectionNode(
          ListNode(4, ListNode(1, sharedN)),
          ListNode(5, ListNode(6, ListNode(1, sharedN)))
        )
        .x == 8
    )
    println(
      Solution
        .getIntersectionNode(
          ListNode(2, ListNode(6, ListNode(4, null))),
          ListNode(1, ListNode(5, null))
        ) == null
    )
  }
}
