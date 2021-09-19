package example

/** Binary Tree Zigzag Level Order Traversal
  * Given the root of a binary tree, return the zigzag level order traversal of its nodes' values. (i.e., from left to right, then right to left for the next level and alternate between).
  *
  * Constraints:
  *
  * The number of nodes in the tree is in the range [0, 2000].
  * -100 <= Node.val <= 100
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/108/trees-and-graphs/787/
  */
trait BinaryTreeZigzagLevelOrderTraversal {

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
    def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {
      def loop(
          node: TreeNode,
          zig: Boolean
      ): List[List[Int]] = {
        if (node == null) List.empty
        else {
          var innterZig = !zig
          val ret =
            loop(node.left, !zig).zipAll(loop(node.right, !zig), Nil, Nil).map {
              case (l, r) =>
                innterZig = !innterZig
                if (innterZig) l ::: r else r ::: l
            }
          List(node.value) +: ret
        }
      }
      if (root == null) List.empty
      else loop(root, false)
    }
  }

  def run() = {
    println(Solution.zigzagLevelOrder(new TreeNode(1)))
    println(
      Solution.zigzagLevelOrder(
        new TreeNode(
          3,
          new TreeNode(9),
          new TreeNode(20, new TreeNode(15), new TreeNode(7))
        )
      )
    )
    println(
      Solution.zigzagLevelOrder(
        new TreeNode(
          1,
          new TreeNode(2, new TreeNode(4)),
          new TreeNode(3, new TreeNode(5))
        )
      )
    )
  }
}
