package example

/** Given an m x n grid of characters board and a string word, return true if word exists in the grid.
  * The word can be constructed from letters of sequentially adjacent cells, where adjacent cells are horizontally or vertically neighboring. The same letter cell may not be used more than once.
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/109/backtracking/797/
  */
trait WordSearch {
  object Solution {
    def startPos(board: Array[Array[Char]], target: Char): Array[(Int, Int)] = {
      for {
        (r, m) <- board.zipWithIndex
        (c, n) <- r.zipWithIndex
        if c == target
      } yield (m, n)
    }
    def exist(board: Array[Array[Char]], word: String): Boolean = {
      def loop(
          start: (Int, Int),
          word: String
      ): Boolean = ???

      word.toList match {
        case head :: tail =>
          (for {
            p <- startPos(board, head)
            if loop(p, tail.mkString)
          } yield ()).nonEmpty
        case Nil => false
      }
    }
  }

  def run() = {}
}
