package easy

/** Binary Tree Preorder Traversal
  * Given the root of a binary tree, return the preorder traversal of its nodes' values.
  *
  * Constraints:
  *
  * The number of nodes in the tree is in the range [0, 100].
  * -100 <= Node.val <= 100
  */
trait BinaryTreePreorderTraversal {
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
  /** use stack and iteratively
    */
  object Solution {
    def preorderTraversal(root: TreeNode): List[Int] = {
      if (root == null) Nil
      else {
        val S = collection.mutable.Stack.empty[TreeNode]
        val ret = collection.mutable.ListBuffer.empty[Int]
        var cur = root
        while (cur != null || S.nonEmpty) {
          while (cur != null) {
            ret.addOne(cur.value)
            S.push(cur.right)
            cur = cur.left
          }
          cur = S.pop()
        }
        ret.toList
      }
    }
  }

  /** recursively
    */
  object Solution2 {
    def preorderTraversal(root: TreeNode): List[Int] = {
      if (root == null) Nil
      else
        root.value +: preorderTraversal(root.left) ::: preorderTraversal(
          root.right
        )
    }
  }

  def run() = {
    println(
      Solution
        .preorderTraversal(
          new TreeNode(1, null, new TreeNode(2, new TreeNode(3)))
        )
      // .sameElements(List(1, 2, 3))
    )
  }
}
