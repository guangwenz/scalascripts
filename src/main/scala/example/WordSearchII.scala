package example

/** Word Search II
  * Given an m x n board of characters and a list of strings words, return all words on the board.
  *
  * Each word must be constructed from letters of sequentially adjacent cells, where adjacent cells are horizontally or vertically neighboring. The same letter cell may not be used more than once in a word.
  *
  * Constraints:
  *
  * m == board.length
  * n == board[i].length
  * 1 <= m, n <= 12
  * board[i][j] is a lowercase English letter.
  * 1 <= words.length <= 3 * 10^4
  * 1 <= words[i].length <= 10
  * words[i] consists of lowercase English letters.
  * All the strings of words are unique.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/119/backtracking/853/
  */
trait WordSearchII {
  object Solution {
    def findWords(
        board: Array[Array[Char]],
        words: Array[String]
    ): List[String] = {
      def findWord(word: String): Boolean = {
        val memo =
          collection.mutable.Map.empty[((Int, Int), String, Int), Boolean]
        def doFind(
            start: (Int, Int),
            word: String,
            visited: Map[(Int, Int), Boolean]
        ): Boolean = {
          val key = (start, word, visited.hashCode())
          if (memo.contains(key)) {
            memo(key)
          } else {
            val ret =
              if (word.isEmpty()) true
              else {
                val (r, c) = start
                val p = for {
                  neibour <- List(
                    (r, c - 1),
                    (r, c + 1),
                    (r - 1, c),
                    (r + 1, c)
                  )
                  (nr, nc) = neibour
                  if nr >= 0 && nr < board.length && nc >= 0 && nc < board.head.length && !visited
                    .contains(neibour) && board(nr)(
                    nc
                  ) == word.head && doFind(
                    neibour,
                    word.tail,
                    visited + (neibour -> true)
                  )
                } yield neibour
                p.nonEmpty
              }
            memo.put(key, ret)
            ret
          }
        }
        val starts = for {
          r <- 0 until board.length
          c <- 0 until board(r).length
          ch = board(r)(c)
          if ch == word.head
        } yield (r, c)
        starts.exists(s => doFind(s, word.tail, Map(s -> true)))
      }
      val ret = words.filter(findWord).toList
      println(ret)
      ret
    }
  }

  def run() = {
    println(
      Solution
        .findWords(
          Array(
            Array('o', 'a', 'a', 'n'),
            Array('e', 't', 'a', 'e'),
            Array('i', 'h', 'k', 'r'),
            Array('i', 'f', 'l', 'v')
          ),
          Array("oath", "pea", "eat", "rain")
        )
        .sameElements(Array("eat", "oath"))
    )
    println(
      Solution
        .findWords(
          Array(
            Array('a', 'b'),
            Array('c', 'd')
          ),
          Array("abcb")
        )
        .isEmpty
    )
    println(
      Solution
        .findWords(
          Array(
            Array('a', 'a')
          ),
          Array("aaa")
        )
        .isEmpty
    )
    println(
      Solution
        .findWords(
          Array(
            Array('a', 'b', 'e'),
            Array('b', 'c', 'd')
          ),
          Array("abcdeb")
        )
        .sameElements(Array("abcdeb"))
    )
    println(
      Solution
        .findWords(
          Array(
            Array('a', 'b'),
            Array('a', 'a')
          ),
          Array("aba", "baa", "bab", "aaab", "aaa", "aaaa", "aaba")
        )
        .sameElements(Array("aba", "aaa", "aaab", "baa", "aaba"))
    )
  }
}
