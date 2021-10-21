package medium

/** Remove Nth Node From End of List
  * Given the head of a linked list, remove the nth node from the end of the list and return its head.
  *
  * Constraints:
  *
  * The number of nodes in the list is sz.
  * 1 <= sz <= 30
  * 0 <= Node.val <= 100
  * 1 <= n <= sz
  *
  * https://leetcode.com/problems/remove-nth-node-from-end-of-list/
  */
trait RemoveNthNodeFromEndofList {
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
    def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
      var cnt = 0
      var current = head
      while (current != null) {
        cnt += 1
        current = current.next
      }
      cnt - n match {
        case 0 => head.next
        case i =>
          cnt = 0
          current = head
          while (cnt < i - 1) {
            cnt += 1
            current = current.next
          }
          if (current.next != null) {
            current.next = current.next.next
          }
          head
      }
    }
  }

  def run() = {
    println(
      Solution
        .removeNthFromEnd(
          new ListNode(
            1,
            new ListNode(2, new ListNode(3, new ListNode(4, new ListNode(5))))
          ),
          2
        )
        .x == 1
    )
    println(Solution.removeNthFromEnd(new ListNode(1), 1) == null)
    println(
      Solution.removeNthFromEnd(new ListNode(1, new ListNode(2)), 1).x == 1
    )
  }
}
