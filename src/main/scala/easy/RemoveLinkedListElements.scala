package easy

/** Remove Linked List Elements
  * Given the head of a linked list and an integer val, remove all the nodes of the linked list that has Node.val == val, and return the new head.
  *
  *  Constraints:
  *
  * The number of nodes in the list is in the range [0, 10^4].
  * 1 <= Node.val <= 50
  * 0 <= val <= 50
  *
  * https://leetcode.com/problems/remove-linked-list-elements/
  */
trait RemoveLinkedListElements {

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
    def removeElements(head: ListNode, `val`: Int): ListNode = {
      if (head == null) null
      else if (head.x == `val`) removeElements(head.next, `val`)
      else {
        var prev = head
        var current = head.next
        while (current != null) {
          if (current.x == `val`) {
            prev.next = current.next
          } else {
            prev = current
          }
          current = current.next
        }
        head
      }
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
      Solution.removeElements(
        new ListNode(1, new ListNode(2, new ListNode(2, new ListNode(1)))),
        2
      )
    )
  }
}
