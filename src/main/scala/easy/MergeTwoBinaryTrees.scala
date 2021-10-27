package easy

/** Merge Two Binary Trees
  *
  * You are given two binary trees root1 and root2.
  *
  * Imagine that when you put one of them to cover the other, some nodes of the two trees are overlapped while the others are not. You need to merge the two trees into a new binary tree. The merge rule is that if two nodes overlap, then sum node values up as the new value of the merged node. Otherwise, the NOT null node will be used as the node of the new tree.
  *
  * Return the merged tree.
  *
  * Note: The merging process must start from the root nodes of both trees.
  *
  * Constraints:
  *
  * The number of nodes in both trees is in the range [0, 2000].
  * -10^4 <= Node.val <= 10^4
  * https://leetcode.com/problems/merge-two-binary-trees/
  */
trait MergeTwoBinaryTrees {

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
    def mergeTrees(root1: TreeNode, root2: TreeNode): TreeNode = {
      if (root1 == null) root2
      else if (root2 == null) root1
      else {
        new TreeNode(
          root1.value + root2.value,
          mergeTrees(root1.left, root2.left),
          mergeTrees(root1.right, root2.right)
        )
      }
    }
  }

  def show(root: TreeNode): Unit = {
    if (root != null) {
      println(root.value)
      show(root.left)
      show(root.right)
    }
  }
  def run() = {
    show(
      Solution.mergeTrees(
        new TreeNode(1, new TreeNode(3, new TreeNode(5)), new TreeNode(2)),
        new TreeNode(
          2,
          new TreeNode(1, null, new TreeNode(4)),
          new TreeNode(3, null, new TreeNode(7))
        )
      )
    )
  }
}
