package example

/** Sort List
  * Given the head of a linked list, return the list after sorting it in ascending order.
  *
  * Constraints:
  *
  * The number of nodes in the list is in the range [0, 5 * 10^4].
  * -10^5 <= Node.val <= 10^5
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/117/linked-list/840/
  */
trait SortList {
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
    def find(head: ListNode)(fn: ListNode => Boolean): Option[ListNode] = {
      var cur = head
      while (cur != null) {
        if (fn(cur)) return Some(cur)
        cur = cur.next
      }
      None
    }
    def insert(node: ListNode, target: ListNode): ListNode = {
      if (node == null) return target
      if (target == null) return node
      if (node.x < target.x) {
        node.next = target
        return node
      } else {
        find(target) { n =>
          n.x <= node.x && (n.next == null || n.next.x > node.x)
        }.map { n =>
          val tmp = n.next
          n.next = node
          node.next = tmp
        }
        target
      }
    }
    def sortList(head: ListNode): ListNode = {
      if (head == null) return null
      val tail = sortList(head.next)
      head.next = null
      insert(head, tail)
    }
  }

  def show(head: ListNode): String = {
    var cur = head
    var ret = ""
    while (cur != null) {
      ret += cur.x + ","
      cur = cur.next
    }
    ret.init
  }
  def run() = {
    println(
      show(
        Solution.sortList(
          new ListNode(
            -1,
            new ListNode(5, new ListNode(3, new ListNode(4, new ListNode(0))))
          )
        )
      ) == "-1,0,3,4,5"
    )
    println(show(Solution.sortList(null)) == "")
  }
}
