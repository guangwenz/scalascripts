package easy

/** Middle of the Linked List
  * Given the head of a singly linked list, return the middle node of the linked list.
  *
  * If there are two middle nodes, return the second middle node.
  * Constraints:
  *
  * The number of nodes in the list is in the range [1, 100].
  * 1 <= Node.val <= 100
  * https://leetcode.com/problems/middle-of-the-linked-list/
  */
trait MiddleoftheLinkedList {
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
    def middleNode(head: ListNode): ListNode = {
      var cnt = 0
      var current = head
      while (current != null) {
        current = current.next
        cnt += 1
      }
      val middle = cnt / 2
      current = head
      var idx = 0
      while (idx != middle) {
        idx += 1
        current = current.next
      }
      current
    }
  }

  def run() = {
    println(
      Solution
        .middleNode(
          new ListNode(
            1,
            new ListNode(
              2,
              new ListNode(3, new ListNode(4, new ListNode(5)))
            )
          )
        )
        .x == 3
    )
    println(
      Solution
        .middleNode(
          new ListNode(
            1,
            new ListNode(
              2,
              new ListNode(3, new ListNode(4, new ListNode(5, new ListNode(6))))
            )
          )
        )
        .x == 4
    )
  }

}
