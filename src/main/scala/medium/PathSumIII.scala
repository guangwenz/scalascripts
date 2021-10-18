package medium

/** Path Sum III
  *
  * Given the root of a binary tree and an integer targetSum, return the number of paths where the sum of the values along the path equals targetSum.
  *
  * The path does not need to start or end at the root or a leaf, but it must go downwards (i.e., traveling only from parent nodes to child nodes).
  *
  * Constraints:
  *
  * The number of nodes in the tree is in the range [0, 1000].
  * -10^9 <= Node.val <= 10^9
  * -1000 <= targetSum <= 1000
  *
  * https://leetcode.com/problems/path-sum-iii/
  */
trait PathSumIII {
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
    def pathSum(root: TreeNode, targetSum: Int): Int = {
      def pathSumFrom(root: TreeNode, targetSum: Int): Int = {
        if (root == null) 0
        else if (root.value == targetSum)
          1 + pathSumFrom(root.left, 0) + pathSumFrom(
            root.right,
            0
          )
        else {
          pathSumFrom(root.left, targetSum - root.value) +
            pathSumFrom(root.right, targetSum - root.value)
        }
      }
      val Q = collection.mutable.Queue(root)
      var result = 0
      while (Q.nonEmpty) {
        val current = Q.dequeue()
        if (current != null) {
          result += pathSumFrom(current, targetSum)
          Q.enqueue(current.left, current.right)
        }
      }
      result
    }
  }

  def run() = {
    println(
      Solution.pathSum(
        new TreeNode(
          10,
          new TreeNode(
            5,
            new TreeNode(3, new TreeNode(3), new TreeNode(-2)),
            new TreeNode(2, null, new TreeNode(1))
          ),
          new TreeNode(-3, null, new TreeNode(11))
        ),
        8
      ) == 3
    )
    println(
      Solution.pathSum(
        new TreeNode(
          5,
          new TreeNode(4, new TreeNode(11, new TreeNode(7), new TreeNode(2))),
          new TreeNode(
            8,
            new TreeNode(13),
            new TreeNode(4, new TreeNode(5), new TreeNode(1))
          )
        ),
        22
      ) == 3
    )
    println(
      Solution.pathSum(
        new TreeNode(
          1,
          new TreeNode(-2, new TreeNode(1, new TreeNode(-1)), new TreeNode(3)),
          new TreeNode(
            -3,
            new TreeNode(-2)
          )
        ),
        -1
      ) == 4
    )
    println(
      Solution.pathSum(
        new TreeNode(
          1,
          new TreeNode(-2, new TreeNode(1, new TreeNode(-1)), new TreeNode(3)),
          new TreeNode(
            -3,
            new TreeNode(-2)
          )
        ),
        -1
      ) == 4
    )
    println(
      Solution.pathSum(
        new TreeNode(
          1,
          new TreeNode(-2, new TreeNode(1, new TreeNode(-1)), new TreeNode(3)),
          new TreeNode(
            -3,
            new TreeNode(-2)
          )
        ),
        1
      ) == 3
    )
  }
}
