package example

/** Lowest Common Ancestor of a Binary Tree
  * Given a binary tree, find the lowest common ancestor (LCA) of two given nodes in the tree.
  *
  * According to the definition of LCA on Wikipedia: “The lowest common ancestor is defined between two nodes p and q as the lowest node in T that has both p and q as descendants (where we allow a node to be a descendant of itself).”
  *
  * Constraints:
  *
  * The number of nodes in the tree is in the range [2, 10^5].
  * -10^9 <= Node.val <= 10^9
  * All Node.val are unique.
  * p != q
  * p and q will exist in the tree.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/118/trees-and-graphs/844/
  */
trait LowestCommonAncestorofBinaryTree {

  class TreeNode(var _value: Int) {
    var value: Int = _value
    var left: TreeNode = null
    var right: TreeNode = null
  }
  object TreeNode {
    def apply(value: Int, left: TreeNode, right: TreeNode): TreeNode = {
      val node = new TreeNode(value)
      node.left = left
      node.right = right
      node
    }
  }

  /** Definition for a binary tree node.
    * class TreeNode(var _value: Int) {
    *   var value: Int = _value
    *   var left: TreeNode = null
    *   var right: TreeNode = null
    * }
    */
  object Solution {
    def lowestCommonAncestor(
        root: TreeNode,
        p: TreeNode,
        q: TreeNode
    ): TreeNode = {
      def isDecent(parent: TreeNode, node: TreeNode): Boolean = {
        val Q = collection.mutable.Queue(parent)
        while (Q.nonEmpty) {
          val n = Q.dequeue()
          if (n.value == node.value) return true
          else {
            if (n.left != null) Q.enqueue(n.left)
            if (n.right != null) Q.enqueue(n.right)
          }
        }
        false
      }

      if (isDecent(p, q)) p
      else if (isDecent(q, p)) q
      else {
        var lcs = root
        val queue = collection.mutable.Queue(root)
        while (queue.nonEmpty) {
          val n = queue.dequeue
          if (isDecent(n, p) && isDecent(n, q)) lcs = n
          if (n == p || n == q) return lcs
          if (n.left != null) queue.enqueue(n.left)
          if (n.right != null) queue.enqueue(n.right)
        }
        lcs
      }
    }
  }

  def run() = {
    val root = TreeNode(
      3,
      TreeNode(
        5,
        new TreeNode(6),
        TreeNode(2, new TreeNode(7), new TreeNode(4))
      ),
      TreeNode(1, new TreeNode(0), new TreeNode(8))
    )
    println(
      Solution
        .lowestCommonAncestor(root, new TreeNode(5), new TreeNode(1))
        .value == 3
    )
    val root2 = TreeNode(1, new TreeNode(2), null)
    println(
      Solution
        .lowestCommonAncestor(root2, new TreeNode(1), new TreeNode(2))
        .value == 1
    )
    val q = new TreeNode(4)
    val p = TreeNode(
      5,
      new TreeNode(6),
      TreeNode(2, new TreeNode(7), q)
    )
    val root3 = TreeNode(
      3,
      p,
      TreeNode(1, new TreeNode(0), new TreeNode(8))
    )
    println(
      Solution
        .lowestCommonAncestor(root3, p, q)
        .value == 5
    )
  }

}
