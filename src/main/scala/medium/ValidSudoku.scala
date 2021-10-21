package medium

/** Valid Sudoku
  * Determine if a 9 x 9 Sudoku board is valid. Only the filled cells need to be validated according to the following rules:
  *
  * Each row must contain the digits 1-9 without repetition.
  * Each column must contain the digits 1-9 without repetition.
  * Each of the nine 3 x 3 sub-boxes of the grid must contain the digits 1-9 without repetition.
  * Note:
  *
  * A Sudoku board (partially filled) could be valid but is not necessarily solvable.
  * Only the filled cells need to be validated according to the mentioned rules.
  *
  * Constraints:
  *
  * board.length == 9
  * board[i].length == 9
  * board[i][j] is a digit 1-9 or '.'.
  *
  * https://leetcode.com/problems/valid-sudoku/
  */
trait ValidSudoku {
  object Solution {
    def isValidSudoku(board: Array[Array[Char]]): Boolean = {

      val rowValid =
        (0 to 8).forall { r =>
          val nums = board(r).collect { case x if x != '.' => x }
          nums.distinct.length == nums.length
        }
      val colValid = {
        val cols = for {
          c <- 0 to 8
          col = (0 to 8).map(r => board(r)(c))
        } yield col
        cols.forall { c =>
          val nums = c.collect {
            case x if x != '.' => x
          }
          nums.length == nums.distinct.length
        }
      }
      val threeboxValid = {
        val invalidBox = for {
          r <- 0 to 6 by 3
          c <- 0 to 6 by 3
          box = {
            for {
              rb <- r to r + 2
              cb <- c to c + 2
              v = board(rb)(cb)
              if v != '.'
            } yield v
          }
          //   _ = println(s"box from ($r,$c) is $box")
          if box.distinct.length != box.length
        } yield box
        invalidBox.isEmpty
      }
      //   println(s"row $rowValid, col $colValid, box $threeboxValid")
      rowValid && colValid && threeboxValid
    }
  }

  def run() = {
    println(
      Solution.isValidSudoku(
        Array(
          Array('5', '3', '.', '.', '7', '.', '.', '.', '.'),
          Array('6', '.', '.', '1', '9', '5', '.', '.', '.'),
          Array('.', '9', '8', '.', '.', '.', '.', '6', '.'),
          Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
          Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
          Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
          Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
          Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
          Array('.', '.', '.', '.', '8', '.', '.', '7', '9')
        )
      ) == true
    )
    println(
      Solution.isValidSudoku(
        Array(
          Array('8', '3', '.', '.', '7', '.', '.', '.', '.'),
          Array('6', '.', '.', '1', '9', '5', '.', '.', '.'),
          Array('.', '9', '8', '.', '.', '.', '.', '6', '.'),
          Array('8', '.', '.', '.', '6', '.', '.', '.', '3'),
          Array('4', '.', '.', '8', '.', '3', '.', '.', '1'),
          Array('7', '.', '.', '.', '2', '.', '.', '.', '6'),
          Array('.', '6', '.', '.', '.', '.', '2', '8', '.'),
          Array('.', '.', '.', '4', '1', '9', '.', '.', '5'),
          Array('.', '.', '.', '.', '8', '.', '.', '7', '9')
        )
      ) == false
    )
  }
}
