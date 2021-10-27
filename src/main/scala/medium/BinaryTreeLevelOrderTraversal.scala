package medium

/** Binary Tree Level Order Traversal
  *
  * Given the root of a binary tree, return the level order traversal of its nodes' values. (i.e., from left to right, level by level).
  *
  * Constraints:
  *
  * The number of nodes in the tree is in the range [0, 2000].
  * -1000 <= Node.val <= 1000
  */
trait BinaryTreeLevelOrderTraversal {
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
    def levelOrder(root: TreeNode): List[List[Int]] = {
      if (root == null) Nil
      else {
        val Q = collection.mutable.Queue((1, root))
        val levelMap = collection.mutable.Map.empty[Int, List[Int]]
        while (Q.nonEmpty) {
          val (level, curr) = Q.dequeue()
          levelMap.put(
            level,
            levelMap.getOrElse(level, List.empty[Int]) :+ curr.value
          )
          if (curr.left != null) {
            Q.enqueue((level + 1, curr.left))
          }
          if (curr.right != null) {
            Q.enqueue((level + 1, curr.right))
          }
        }
        levelMap.keys.toList.sorted.map(levelMap(_))
      }
    }
  }

  def run() = {
    println(
      Solution.levelOrder(
        new TreeNode(
          3,
          new TreeNode(9),
          new TreeNode(20, new TreeNode(15), new TreeNode(7))
        )
      )
    )
    println(Solution.levelOrder(new TreeNode(1)))
    println(Solution.levelOrder(null))
  }
}
