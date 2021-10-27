package easy

/** Binary Tree Postorder Traversal
  *
  * Given the root of a binary tree, return the postorder traversal of its nodes' values.
  *
  * Constraints:
  *
  * The number of the nodes in the tree is in the range [0, 100].
  * -100 <= Node.val <= 100
  * https://leetcode.com/problems/binary-tree-postorder-traversal/
  */
trait BinaryTreePostorderTraversal {
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
  /** recursive
    */
  object Solution {
    def postorderTraversal(root: TreeNode): List[Int] = {
      if (root == null) Nil
      else {
        postorderTraversal(root.left) ::: postorderTraversal(
          root.right
        ) ::: List(
          root.value
        )
      }
    }
  }

  def run() = {
    println(
      Solution.postorderTraversal(
        new TreeNode(1, null, new TreeNode(2, new TreeNode(3)))
      )
    )
  }
}
