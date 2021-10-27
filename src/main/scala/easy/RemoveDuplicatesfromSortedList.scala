package easy

/** Remove Duplicates from Sorted List
  * Given the head of a sorted linked list, delete all duplicates such that each element appears only once. Return the linked list sorted as well.
  *
  * Constraints:
  *
  * The number of nodes in the list is in the range [0, 300].
  * -100 <= Node.val <= 100
  * The list is guaranteed to be sorted in ascending order.
  */
trait RemoveDuplicatesfromSortedList {

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
    def deleteDuplicates(head: ListNode): ListNode = {
      if (head == null) null
      else {
        var prev = head
        var current = head.next
        while (current != null) {
          if (current.x == prev.x) {
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
      Solution.deleteDuplicates(
        new ListNode(1, new ListNode(1, new ListNode(2)))
      )
    )
    show(
      Solution.deleteDuplicates(
        new ListNode(
          1,
          new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(3))))
        )
      )
    )
  }
}
