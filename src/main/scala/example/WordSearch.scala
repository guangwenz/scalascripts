package example

/** Given an m x n grid of characters board and a string word, return true if word exists in the grid.
  * The word can be constructed from letters of sequentially adjacent cells, where adjacent cells are horizontally or vertically neighboring. The same letter cell may not be used more than once.
  *
  * Constraints:
  *
  * m == board.length
  * n = board[i].length
  * 1 <= m, n <= 6
  * 1 <= word.length <= 15
  * board and word consists of only lowercase and uppercase English letters.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/109/backtracking/797/
  */
trait WordSearch {
  object Solution {
    def exist(board: Array[Array[Char]], word: String): Boolean = {
      def loop(
          pos: (Int, Int),
          word: String,
          visited: List[(Int, Int)]
      ): Boolean = {
        if (word.isEmpty())
          true
        else {
          val (i, j) = pos
          val p = for {
            adj <- List((i, j - 1), (i, j + 1), (i - 1, j), (i + 1, j))
            (i, j) = adj
            if i >= 0 && i < board.size && j >= 0 && j < board.head.size && board(
              i
            )(j) == word.head && !visited.contains(adj) && {
              loop(
                adj,
                word.tail,
                adj +: visited
              )
            }
          } yield ()
          p.nonEmpty
        }
      }

      val p = for {
        r <- 0 until board.length
        c <- 0 until board.head.length
        if board(r)(c) == word.head
      } yield (r, c)
      p.exists(pos => loop(pos, word.tail, List(pos)))
    }
  }
  def run() = {
    println(
      Solution.exist(
        Array(
          Array('A', 'B', 'C', 'E'),
          Array('S', 'F', 'C', 'S'),
          Array('A', 'D', 'E', 'E')
        ),
        "ABCCED"
      ) == true
    )
    println(
      Solution.exist(
        Array(
          Array('A', 'B', 'C', 'E'),
          Array('S', 'F', 'C', 'S'),
          Array('A', 'D', 'E', 'E')
        ),
        "SEE"
      ) == true
    )
    println(
      Solution.exist(
        Array(
          Array('A', 'B', 'C', 'E'),
          Array('S', 'F', 'C', 'S'),
          Array('A', 'D', 'E', 'E')
        ),
        "ABCB"
      ) == false
    )
  }
}
