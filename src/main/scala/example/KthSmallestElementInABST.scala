package example

/** Kth Smallest Element in a BST
  * Given the root of a binary search tree, and an integer k, return the kth (1-indexed) smallest element in the tree.
  *
  * Constraints:
  *
  * The number of nodes in the tree is n.
  * 1 <= k <= n <= 104
  * 0 <= Node.val <= 104
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/108/trees-and-graphs/790/
  */
trait KthSmallestElementInABST {
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
    def inorder(root: TreeNode): List[Int] = {
      if (root == null) Nil
      else {
        inorder(root.left) ::: List(root.value) ::: inorder(root.right)
      }
    }
    def kthSmallest(root: TreeNode, k: Int): Int = {
      inorder(root)(k - 1)
    }
  }

  def run() = {
    println(
      Solution.kthSmallest(
        new TreeNode(
          3,
          new TreeNode(1, null, new TreeNode(2)),
          new TreeNode(4)
        ),
        1
      ) == 1
    )
    println(
      Solution.kthSmallest(
        new TreeNode(
          5,
          new TreeNode(3, new TreeNode(2, new TreeNode(1)), new TreeNode(4)),
          new TreeNode(6)
        ),
        3
      ) == 3
    )
  }
}
