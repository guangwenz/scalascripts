package easy

/** Given the root of a binary tree, return the inorder traversal of its nodes' values.
  *
  * Constraints:
  *
  * The number of nodes in the tree is in the range [0, 100].
  * -100 <= Node.val <= 100
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/108/trees-and-graphs/786/
  */
trait BinaryTreeInorderTraversal {

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

  /** iterative with stack
    */
  object Solution {
    def inorderTraversal(root: TreeNode): List[Int] = {
      val S = collection.mutable.Stack.empty[TreeNode]
      val ret = collection.mutable.ListBuffer.empty[Int]
      var cur = root
      while (cur != null || S.nonEmpty) {
        while (cur != null) {
          S.push(cur)
          cur = cur.left
        }
        cur = S.pop()
        ret.addOne(cur.value)
        cur = cur.right
      }
      ret.toList
    }
  }

  /** classical recurive
    */
  object Solution2 {
    def inorderTraversal(root: TreeNode): List[Int] = {
      if (root == null) Nil
      else {
        inorderTraversal(root.left) ::: List(root.value) ::: inorderTraversal(
          root.right
        )
      }
    }
  }

  def run() = {
    println(
      Solution.inorderTraversal(
        new TreeNode(1, null, new TreeNode(2, new TreeNode(3)))
      )
    )
  }
}
