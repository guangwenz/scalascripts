package easy

/** Symmetric Tree
  *
  * Given the root of a binary tree, check whether it is a mirror of itself (i.e., symmetric around its center).
  *
  * Constraints:
  *
  * The number of nodes in the tree is in the range [1, 1000].
  * -100 <= Node.val <= 100
  *
  * https://leetcode.com/problems/symmetric-tree/
  */
trait SymmetricTree {
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
    def isSymmetric(root: TreeNode): Boolean = {
      def isSymmetric(l: TreeNode, r: TreeNode): Boolean = {
        if (l == null) r == null
        else if (r == null) l == null
        else {
          l.value == r.value && isSymmetric(l.left, r.right) && isSymmetric(
            l.right,
            r.left
          )
        }
      }
      isSymmetric(root.left, root.right)
    }
  }

  def run() = {}
}
