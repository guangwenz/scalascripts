package easy

/** Linked List Cycle
  *
  * Given head, the head of a linked list, determine if the linked list has a cycle in it.
  *
  * There is a cycle in a linked list if there is some node in the list that can be reached again by continuously following the next pointer. Internally, pos is used to denote the index of the node that tail's next pointer is connected to. Note that pos is not passed as a parameter.
  *
  * Return true if there is a cycle in the linked list. Otherwise, return false.
  *
  * Constraints:
  *
  * The number of the nodes in the list is in the range [0, 104].
  * -10^5 <= Node.val <= 10^5
  * pos is -1 or a valid index in the linked-list.
  * https://leetcode.com/problems/linked-list-cycle/
  */
trait LinkedListCycle {

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
  }

  object ListNode {
    def apply(x: Int, next: ListNode = null): ListNode = {
      val ret = new ListNode(x)
      ret.next = next
      ret
    }
  }

  /** Definition for singly-linked list.
    * class ListNode(var _x: Int = 0) {
    *   var next: ListNode = null
    *   var x: Int = _x
    * }
    */

  object Solution {
    def hasCycle(head: ListNode): Boolean = {
      if (head == null) false
      else {
        val visited = collection.mutable.Map.empty[ListNode, Boolean]
        var currrent = head
        while (currrent != null) {
          if (visited.contains(currrent)) return true
          else visited.put(currrent, true)
          currrent = currrent.next
        }
        false
      }
    }
  }

  def run() = {
    val two = new ListNode(2)
    two.next = ListNode(0, ListNode(-4, two))
    println(
      Solution.hasCycle(
        ListNode(3, two)
      ) == true
    )

    val one2 = new ListNode(1)
    val two2 = new ListNode(2)
    one2.next = two2
    two2.next = one2
    println(Solution.hasCycle(one2) == true)

    println(Solution.hasCycle(ListNode(1)) == false)
  }

}
