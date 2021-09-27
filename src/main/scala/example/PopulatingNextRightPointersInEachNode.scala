package example

/** Populating Next Right Pointers in Each Node
  * You are given a perfect binary tree where all leaves are on the same level, and every parent has two children. The binary tree has the following definition:
  *
  * struct Node {
  *  int val;
  *  Node *left;
  *  Node *right;
  *  Node *next;
  * }
  * Populate each next pointer to point to its next right node. If there is no next right node, the next pointer should be set to NULL.
  *
  * Initially, all next pointers are set to NULL.
  *
  * Constraints:
  *
  * The number of nodes in the tree is in the range [0, 212 - 1].
  * -1000 <= Node.val <= 1000
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/108/trees-and-graphs/789/
  */
trait PopulatingNextRightPointersInEachNode {
  class Node(var _value: Int) {
    var value: Int = _value
    var left: Node = null
    var right: Node = null
    var next: Node = null
  }
  object Node {
    def apply(value: Int, left: Node, right: Node): Node = {
      val n = new Node(value)
      n.left = left
      n.right = right
      n
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
    def leveled(root: Node): List[List[Node]] = {
      if (root == null) Nil
      else {
        val merged =
          leveled(root.left)
            .zipAll(leveled(root.right), Nil, Nil)
            .map(i => i._1 ::: i._2)
        List(root) +: merged
      }
    }
    def connect(root: Node): Node = {
      leveled(root).foreach(_.foldLeft(Option.empty[Node]) { case (s, n) =>
        s match {
          case Some(value) =>
            value.next = n
            Some(n)
          case None =>
            Some(n)
        }
      })
      root
    }
  }

  def preorder(root: Node): List[Int] = {
    if (root == null) Nil
    else {
      root.value +: preorder(root.left) ::: preorder(root.right)
    }
  }
  def run() = {
    val tree = Solution.connect(
      Node(
        1,
        Node(2, new Node(4), new Node(5)),
        Node(3, new Node(6), new Node(7))
      )
    )
    println(Solution.leveled(tree).map(_.map(_.value)).mkString(","))
    // println(preorder(tree))
    val tree2 = Solution.connect(null)
    println(preorder(tree2))
  }
}
