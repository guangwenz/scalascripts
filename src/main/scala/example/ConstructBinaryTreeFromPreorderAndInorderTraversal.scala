package example

/** Given two integer arrays preorder and inorder where preorder is the preorder traversal of a binary tree and inorder is the inorder traversal of the same tree, construct and return the binary tree.
  *
  * Constraints:
  *
  * 1 <= preorder.length <= 3000
  * inorder.length == preorder.length
  * -3000 <= preorder[i], inorder[i] <= 3000
  * preorder and inorder consist of unique values.
  * Each value of inorder also appears in preorder.
  * preorder is guaranteed to be the preorder traversal of the tree.
  * inorder is guaranteed to be the inorder traversal of the tree.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/108/trees-and-graphs/788/
  */
trait ConstructBinaryTreeFromPreorderAndInorderTraversal {
  class TreeNode(
      _value: Int = 0,
      _left: TreeNode = null,
      _right: TreeNode = null
  ) {
    var value: Int = _value
    var left: TreeNode = _left
    var right: TreeNode = _right
  }

  /** Definition for a binary tree node.
    * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    *   var value: Int = _value
    *   var left: TreeNode = _left
    *   var right: TreeNode = _right
    * }
    */
  object Solution {
    def buildTree(preorder: Array[Int], inorder: Array[Int]): TreeNode = {
      if (preorder.isEmpty) null
      else {
        val head = preorder.head
        val node = new TreeNode(head)
        if (preorder.length == 1) node
        else {
          val (l, r) = {
            val x = inorder.splitAt(inorder.indexOf(head))
            (x._1, x._2.tail)
          }
          val (preL, preR) = preorder.tail.partition(!r.contains(_))
          if (l.nonEmpty) {
            val lNode = buildTree(preL, l)
            node.left = lNode
          }
          if (r.nonEmpty) {
            val rNode = buildTree(preR, r)
            node.right = rNode
          }
        }
        node
      }
    }
  }

  def preorderTree(node: TreeNode): List[Int] = {
    if (node == null) List.empty
    else {
      node.value +: (preorderTree(node.left) ::: preorderTree(node.right))
    }
  }

  def run() = {
    val tree =
      Solution.buildTree(Array(3, 9, 20, 15, 7), Array(9, 3, 15, 20, 7))
    println(preorderTree(tree))
  }
}
