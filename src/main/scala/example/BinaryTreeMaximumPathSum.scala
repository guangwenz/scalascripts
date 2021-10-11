package example

/** Binary Tree Maximum Path Sum
  * A path in a binary tree is a sequence of nodes where each pair of adjacent nodes in the sequence has an edge connecting them. A node can only appear in the sequence at most once. Note that the path does not need to pass through the root.
  *
  * The path sum of a path is the sum of the node's values in the path.
  *
  * Given the root of a binary tree, return the maximum path sum of any path.
  *
  * Constraints:
  *
  * The number of nodes in the tree is in the range [1, 3 * 10^4].
  * -1000 <= Node.val <= 1000
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/118/trees-and-graphs/845/
  */

trait BinaryTreeMaximumPathSum {
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
    import collection.mutable.{Map => MMap}
    def maxPathSum(root: TreeNode): Int = {
      val g = MMap.empty[TreeNode, List[TreeNode]]
      def buildGraph(root: TreeNode): Unit = {
        if (root != null) {
          g.put(
            root,
            List(root.left, root.right)
              .filter(_ != null) ::: g.get(root).getOrElse(Nil)
          )
          buildGraph(root.left)
          buildGraph(root.right)
          if (root.left != null) {
            g.put(root.left, List(root) ::: g.get(root.left).getOrElse(Nil))
          }
          if (root.right != null) {
            g.put(root.right, List(root) ::: g.get(root.right).getOrElse(Nil))
          }
        }
      }

      buildGraph(root)

      def maxPathSumInner(root: TreeNode): Int = {
        //DFS traversal
        var result = root.value
        val pathSum = MMap(root -> root.value)
        val visited = collection.mutable.Map.empty[TreeNode, Boolean]
        val S = collection.mutable.Stack(root)
        while (S.nonEmpty) {
          val current = S.pop()
          visited.put(current, true)
          val notVisitedAdjs = for {
            adj <- g(current)
            if !visited.contains(adj)
          } yield {
            S.push(adj)
            pathSum.put(adj, pathSum(current) + adj.value)
            if (pathSum(adj) > result) result = pathSum(adj)
          }
        }
        result
      }
      var result = root.value
      g.foreach { case (k, _) =>
        val ret = maxPathSumInner(k)
        if (ret > result) result = ret
      }
      result
    }
  }

  def printG(g: collection.mutable.Map[TreeNode, List[TreeNode]]): Unit = {
    for {
      (k, v) <- g
      vL = v.map(_.value)
    } yield println(s"${k.value} -> $vL")
  }
  def run() = {
    println(Solution.maxPathSum(new TreeNode(-3)) == -3)
    println(
      Solution.maxPathSum(
        new TreeNode(1, new TreeNode(2), new TreeNode(3))
      ) == 6
    )
    println(
      Solution.maxPathSum(
        new TreeNode(
          -10,
          new TreeNode(9),
          new TreeNode(20, new TreeNode(15), new TreeNode(7))
        )
      ) == 42
    )
    println(
      Solution.maxPathSum(
        new TreeNode(
          1,
          new TreeNode(-2, new TreeNode(1, new TreeNode(-1)), new TreeNode(3)),
          new TreeNode(-3, new TreeNode(-2))
        )
      ) == 3
    )
    println(
      Solution.maxPathSum(
        new TreeNode(
          5,
          new TreeNode(4, new TreeNode(11, new TreeNode(7), new TreeNode(2))),
          new TreeNode(
            8,
            new TreeNode(13),
            new TreeNode(4, null, new TreeNode(1))
          )
        )
      ) == 48
    )
  }
}
