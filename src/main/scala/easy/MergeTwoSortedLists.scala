package easy

/** Merge Two Sorted Lists
  *
  * Merge two sorted linked lists and return it as a sorted list. The list should be made by splicing together the nodes of the first two lists.
  *
  * Constraints:
  *
  * The number of nodes in both lists is in the range [0, 50].
  * -100 <= Node.val <= 100
  * Both l1 and l2 are sorted in non-decreasing order.
  *
  * https://leetcode.com/problems/merge-two-sorted-lists/
  */
trait MergeTwoSortedLists {
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
    def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
      if (l1 == null) l2
      else if (l2 == null) l1
      else {
        l1.x - l2.x match {
          case x if x == 0 =>
            val l2next = l2.next
            l2.next = l1
            l1.next = mergeTwoLists(l1.next, l2next)
            l2
          case x if x < 0 =>
            l1.next = mergeTwoLists(l1.next, l2)
            l1
          case x if x > 0 =>
            l2.next = mergeTwoLists(l1, l2.next)
            l2
        }
      }
    }
  }

  def run() = {
    println(
      Solution.mergeTwoLists(
        new ListNode(1, new ListNode(2, new ListNode(4))),
        new ListNode(1, new ListNode(3, new ListNode(4)))
      )
    )

    println(Solution.mergeTwoLists(null, null) == null)
    println(Solution.mergeTwoLists(null, new ListNode(0)).x == 0)
  }
}
