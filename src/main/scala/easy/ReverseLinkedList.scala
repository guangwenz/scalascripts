package easy

/** Reverse Linked List
  *
  * Given the head of a singly linked list, reverse the list, and return the reversed list.
  *
  * Constraints:
  *
  * The number of nodes in the list is the range [0, 5000].
  * -5000 <= Node.val <= 5000
  *
  * https://leetcode.com/problems/reverse-linked-list/
  */
trait ReverseLinkedList {
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
    def reverseList(head: ListNode): ListNode = {
      var current = head
      var prev: ListNode = null
      while (current != null) {
        val next = current.next
        current.next = prev
        prev = current
        current = next
      }
      prev
    }
  }

  def show(node: ListNode): Unit = {
    var current = node
    while (current != null) {
      println(current.x)
      current = current.next
    }
  }

  def run() = {
    show(
      Solution.reverseList(
        new ListNode(
          1,
          new ListNode(2, new ListNode(3, new ListNode(4, new ListNode(5))))
        )
      )
    )
  }
}
