package medium

/** Populating Next Right Pointers in Each Node
  *
  * You are given a perfect binary tree where all leaves are on the same level, and every parent has two children. The binary tree has the following definition:
  *
  * Populate each next pointer to point to its next right node. If there is no next right node, the next pointer should be set to NULL.
  *
  * Initially, all next pointers are set to NULL.
  *
  * Constraints:
  *
  * The number of nodes in the tree is in the range [0, 2^12 - 1].
  * -1000 <= Node.val <= 1000
  */
trait PopulatingNextRightPointersinEachNode {
  class Node(var _value: Int) {
    var value: Int = _value
    var left: Node = null
    var right: Node = null
    var next: Node = null
    override def toString(): String =
      s"(${value.toString()}, next: ${Option(next).map(_.value)})"
  }
  object Node {
    def apply(v: Int, left: Node = null, right: Node = null): Node = {
      val node = new Node(v)
      node.left = left
      node.right = right
      node
    }
  }

  /** Definition for a Node.
    * class Node(var _value: Int) {
    *   var value: Int = _value
    *   var left: Node = null
    *   var right: Node = null
    *   var next: Node = null
    * }
    */

  object Solution {
    def connect(root: Node): Node = {
      if (root == null) null
      else {
        val levelListMap = collection.mutable.Map.empty[Int, List[Node]]
        val Q = collection.mutable.Queue((root, 1))
        var lastLevel = 1
        while (Q.nonEmpty) {
          val (current, level) = Q.dequeue()
          if (level == lastLevel + 1) {
            val l = levelListMap(lastLevel)
            (0 until l.size - 1).foreach(i => l(i).next = l(i + 1))
            levelListMap.remove(lastLevel)
            levelListMap.put(level, List(current))
          } else {
            levelListMap.put(
              level,
              current +: levelListMap.getOrElse(level, Nil)
            )
          }
          lastLevel = level
          if (current.left != null)
            Q.enqueue((current.right, level + 1), (current.left, level + 1))
        }
        val l = levelListMap(lastLevel)
        (0 until l.size - 1).foreach(i => l(i).next = l(i + 1))
        root
      }
    }
  }

  def showNext(root: Node): Unit = {
    if (root != null) {
      println(root.next)
      if (root.next != null) println(root.next.value)
      showNext(root.left)
      showNext(root.right)
    }
  }

  def run() = {
    println(
      Solution.connect(
        Node(1, Node(2, Node(4), Node(5)), Node(3, Node(6), Node(7)))
      )
    )
  }
}
