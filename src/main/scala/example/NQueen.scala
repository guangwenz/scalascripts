package example

/** n queen problem
  * backtracking
  * https://www.hackerrank.com/challenges/queens-on-board/problem
  */

/** solution 1 with referece from scala book
  */
object Solution1 {
  type Board = List[List[(Int, Int)]]
  object Board {
    def empty: Board = List(List.empty[(Int, Int)])
  }

  def isSafe(queen: (Int, Int), queens: List[(Int, Int)]): Boolean =
    queens.forall(q => !inCheck(queen, q))

  def inCheck(q1: (Int, Int), q2: (Int, Int)): Boolean =
    q1._1 == q2._1 || q1._2 == q2._2 || (q1._1 - q2._1).abs == (q1._2 - q2._2).abs

  def queens(n: Int): Board = {
    def placeQueens(k: Int): Board = {
      if (k == 0) { Board.empty }
      else
        for {
          queens <- placeQueens(k - 1)
          column <- 1 to n
          queen = (k, column)
          if isSafe(queen, queens)
        } yield {
          queen :: queens
        }
    }

    placeQueens(n)
  }
}

object Solution2 {

  def isSafe(queen: (Int, Int), queens: List[(Int, Int)]): Boolean =
    queens.forall(q => !inCheck(queen, q))

  def inCheck(q1: (Int, Int), q2: (Int, Int)): Boolean =
    q1._1 == q2._1 || q1._2 == q2._2 || (q1._1 - q2._1).abs == (q1._2 - q2._2).abs

  def queens(input: List[List[String]]): List[List[String]] = {

    def processCandidate(
        candidate: (Int, Int),
        solutionSpace: List[(Int, Int)]
    ): Option[(Int, Int)] =
      if (isSafe(candidate, solutionSpace)) Some(candidate) else None
    def isDone(pos: (Int, Int)): Boolean =
      pos._1 == input.size && pos._2 == input.head.size
    //search space: next row to place queen
    //constraint: no clashes
    //solution found: queen placed on last row
    def placeQueens(
        candidates: List[(Int, Int)], //next row to place queen
        solutionSpace: List[(Int, Int)]
    ): Option[List[(Int, Int)]] = candidates match {
      case head :: tail =>
        // println(
        //   s"checking $head with solution space $solutionSpace and tail $tail"
        // )
        processCandidate(head, solutionSpace) match {
          case None =>
            // println(s"not available at $head")
            placeQueens(tail, solutionSpace)
          case Some(v) =>
            val newCandidates =
              if (isDone(head)) Nil
              else
                (1 to input.head.size)
                  .map(col => (head._1 + 1, col))
                  .toList
            // println(s"available at $v with tail $newCandidates")
            placeQueens(newCandidates, v +: solutionSpace) match {
              case Some(value) =>
                // println(s"value $value with solution space $solutionSpace")
                Some(value)
              //backtrack
              case None =>
                println(
                  s"no more solutions from candidates $newCandidates, backtracking $tail with solution space $solutionSpace"
                )
                placeQueens(tail, solutionSpace)
            }
        }
      case Nil =>
        // println(s"no more candidates with $solutionSpace")
        if (solutionSpace.size == input.size) Some(solutionSpace)
        else None
    }
    println(placeQueens((1 to input.size).map(i => (1, i)).toList, List.empty))
    ???
  }
}

object Solution3 {
  def isSafe(queen: (Int, Int), queens: List[(Int, Int)]): Boolean =
    queens.forall(q => !inCheck(queen, q))

  def inCheck(q1: (Int, Int), q2: (Int, Int)): Boolean =
    q1._1 == q2._1 || q1._2 == q2._2 || (q1._1 - q2._1).abs == (q1._2 - q2._2).abs

  def queens(input: List[List[String]]): List[List[String]] = {
    def processCandidate(
        candidate: (Int, Int),
        solutionSpace: List[(Int, Int)]
    ): Option[(Int, Int)] =
      if (isSafe(candidate, solutionSpace)) Some(candidate) else None
    //search space: next row to place queen
    //constraint: no clashes
    //solution found: queen placed on last row
    def placeQueens(
        candidates: List[(Int, Int)], //next row to place queen
        currentSolution: List[(Int, Int)],
        solutions: List[List[(Int, Int)]]
    ): List[List[(Int, Int)]] = candidates match {
      case head :: tail =>
        processCandidate(head, currentSolution) match {
          case None =>
            //break constraints, backtracking
            placeQueens(tail, currentSolution, solutions)
          case Some(v) =>
            val newCandidates = (1 to input.head.size)
              .map(col => (head._1 + 1, col))
              .toList

            //next candidates along the path
            placeQueens(
              newCandidates,
              v +: currentSolution,
              solutions
            ) ::: //search for other paths for potential multiple solutions
              placeQueens(
                tail,
                currentSolution,
                solutions
              )
        }
      case Nil =>
        if (currentSolution.size == input.size) currentSolution +: solutions
        else solutions
    }
    println(
      placeQueens(
        (1 to input.size).map(i => (1, i)).toList,
        List.empty,
        List.empty
      ).map(_.mkString("|")).mkString("\n")
    )
    ???
  }
}
trait NQueen {

  def run(): Unit = {

    def solution1() = {
      val n = 10
      val ans = Solution1.queens(n)
      if (ans.isEmpty) println("No solution!")
      else
        for {
          s <- ans
          _ = println()
          l = for {
            r <- Range(1, n + 1)
            l = Range(1, n + 1)
              .map { c =>
                if (s.contains((r, c))) "*" else "."
              }
          } yield {
            println(l)
            l
          }
        } yield ()
    }
  }

  def solution2() = {
    val input = List(
      List(".", ".", ".", "."),
      List(".", ".", ".", "."),
      List(".", ".", ".", "."),
      List(".", ".", ".", ".")
    )
    val input2 = List(
      List(".", ".", ".", ".", "."),
      List(".", ".", ".", ".", "."),
      List(".", ".", ".", ".", "."),
      List(".", ".", ".", ".", "."),
      List(".", ".", ".", ".", ".")
    )
    Solution3.queens(input2)
  }
  solution2()
}
