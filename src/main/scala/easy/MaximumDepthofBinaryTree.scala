package easy

/** Maximum Depth of Binary Tree
  *
  * Given the root of a binary tree, return its maximum depth.
  *
  * A binary tree's maximum depth is the number of nodes along the longest path from the root node down to the farthest leaf node.
  *
  * Constraints:
  *
  * The number of nodes in the tree is in the range [0, 104].
  * -100 <= Node.val <= 100
  *
  * https://leetcode.com/problems/maximum-depth-of-binary-tree/
  */
trait MaximumDepthofBinaryTree {
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
    def maxDepth(root: TreeNode): Int = {
      if (root == null) 0
      else {
        var ret = 1
        val S = collection.mutable.Stack((root, 1))
        while (S.nonEmpty) {
          val (cur, dep) = S.pop()
          ret = math.max(dep, ret)
          if (cur.left != null) S.push((cur.left, dep + 1))
          if (cur.right != null) S.push((cur.right, dep + 1))
        }
        ret
      }
    }
  }
  def run() = {
    println(
      Solution.maxDepth(
        new TreeNode(
          3,
          new TreeNode(9),
          new TreeNode(20, new TreeNode(15), new TreeNode(7))
        )
      ) == 3
    )
    println(Solution.maxDepth(new TreeNode(1, null, new TreeNode(2))) == 2)
    println(Solution.maxDepth(null) == 0)
    println(Solution.maxDepth(new TreeNode(0)) == 1)
  }
}
