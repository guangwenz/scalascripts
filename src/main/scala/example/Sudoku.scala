package example

trait Sudoku {
  def isUnique(board: List[List[Int]]): Boolean = {
    val p = for {
      c <- 0 to board.length - 1
      if (0 to board.length - 1)
        .map(r => board(r)(c))
        .distinct
        .length == board.length
    } yield ()
    board.forall(
      _.distinct.length == board.length
    ) && p.length == board.length
  }

  def sudoku4 = {
    for {
      a11 <- 1 to 4
      a12 <- 1 to 4
      a13 <- 1 to 4
      a14 <- 1 to 4
      if List(a11, a12, a13, a14).distinct.length == 4

      a21 <- 1 to 4
      a22 <- 1 to 4
      a23 <- 1 to 4
      a24 <- 1 to 4
      if List(a21, a22, a23, a24).distinct.length == 4

      a31 <- 1 to 4
      a32 <- 1 to 4
      a33 <- 1 to 4
      a34 <- 1 to 4
      if List(a31, a32, a33, a34).distinct.length == 4

      a41 <- 1 to 4
      a42 <- 1 to 4
      a43 <- 1 to 4
      a44 <- 1 to 4
      if List(a41, a42, a43, a44).distinct.length == 4

      board = List(
        List(a11, a12, a13, a14),
        List(a21, a22, a23, a24),
        List(a31, a32, a33, a34),
        List(a41, a42, a43, a44)
      )
      if isUnique(board)

    } yield board
  }

  def sudoku6 = {
    for {
      a11 <- 1 to 6
      a12 <- 1 to 6
      if a11 + a12 == 9
      a13 <- 1 to 6
      a14 <- 1 to 6
      if a13 / a14 == 2 || a14 / a13 == 2
      a15 <- 1 to 6
      a16 <- 1 to 6
      if a15 / a16 == 2 || a16 / a15 == 2
      if List(a11, a12, a13, a14, a15, a16).distinct.length == 6

      a21 <- 1 to 6
      a22 <- 1 to 6
      a23 <- 1 to 6
      a24 <- 1 to 6
      if a23 / a24 == 2 || a24 / a23 == 2
      a25 <- 1 to 6
      a26 <- 1 to 6
      if List(a21, a22, a23, a24, a25, a26).distinct.length == 6

      a31 <- 1 to 6
      if a21 * a22 * a31 == 5
      a32 <- 1 to 6
      a33 <- 1 to 6
      a34 <- 1 to 6
      a35 <- 1 to 6
      a36 <- 1 to 6
      if a25 * a26 * a36 == 54
      if List(a31, a32, a33, a34, a35, a36).distinct.length == 6
      a41 <- 1 to 6
      a42 <- 1 to 6
      if a32 / a42 == 2 || a42 / a32 == 2
      a43 <- 1 to 6
      if math.abs(a33 - a43) == 3
      a44 <- 1 to 6
      if a34 * a44 == 12
      a45 <- 1 to 6
      if math.abs(a35 - a45) == 5
      a46 <- 1 to 6
      if List(a41, a42, a43, a44, a45, a46).distinct.length == 6

      a51 <- 1 to 6
      a52 <- 1 to 6
      if a41 + a51 + a52 == 11
      a53 <- 1 to 6
      a54 <- 1 to 6
      if math.abs(a53 - a54) == 5
      a55 <- 1 to 6
      a56 <- 1 to 6
      if a46 + a55 + a56 == 14
      if List(a51, a52, a53, a54, a55, a56).distinct.length == 6

      a61 <- 1 to 6
      a62 <- 1 to 6
      a63 <- 1 to 6
      if a61 * a62 * a63 == 18
      a64 <- 1 to 6
      a65 <- 1 to 6
      a66 <- 1 to 6
      if a64 + a65 + a66 == 11
      if List(a61, a62, a63, a64, a65, a66).distinct.length == 6

      board = List(
        List(a11, a12, a13, a14, a15, a16),
        List(a21, a22, a23, a24, a25, a26),
        List(a31, a32, a33, a34, a35, a36),
        List(a41, a42, a43, a44, a45, a46),
        List(a51, a52, a53, a54, a55, a56),
        List(a61, a62, a63, a64, a65, a66)
      )
      if isUnique(board)
    } yield board
  }
  def run() = {
    for {
      an <- sudoku6
    } yield {
      println(an.map(_.mkString(",")).mkString("\n"))
      println
    }

  }

}
