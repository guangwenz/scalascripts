package example

trait Sudoku {
  val input = Array.ofDim[Int](9, 9)
  val sample = """0 0 0 0 0 0 0 0 0
0 0 8 0 0 0 0 4 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 6 0 0 0
0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0
2 0 0 0 0 0 0 0 0
0 0 0 0 0 0 2 0 0
0 0 0 0 0 0 0 0 0"""

  type Board[T] = Seq[Seq[T]]
  def isSolved(input: Board[Int]): Boolean = input.forall(_.forall(_ > 0))
  def isSafe(input: Board[Int], r: Int, c: Int, value: Int) =
    input(r).forall(_ != value) && Range(0, 9).forall(i => input(i)(c) != value)
  def run() {
    val board = for {
      (row, x) <- sample.split("\n").zipWithIndex
      items = row.split(" ").map(_.toInt)
    } yield items.toSeq

    val ret = solution(board)
    // println(input.map(_.mkString(",")).mkString("\n"))
    println(ret)
  }

  def solution(input: Board[Int]): Option[Board[Int]] = {
    val foo = for {
      (r, m) <- input.zipWithIndex
      (c, n) <- r.zipWithIndex if c == 0
      v <- Range(1, 10) if isSafe(input, m, n, v)
      // ret = {
      //   if (isSafe(input, m, n, v)) {
      //     println(s"$m,$n")
      //     input(m)(n) = v
      //     println(input.map(_.mkString(",")).mkString("\n"))
      //     if (isSolved(input)) {
      //       println("solved!")
      //       Some(input)
      //     } else solution(input)
      //   } else None
      // }
    } yield {
      // input(m)(n) = v
      println(input.map(_.mkString(",")).mkString("\n"))
      println()
      if (isSolved(input)) Some(input)
      else solution(input)
    }
    foo.collect { case Some(value) =>
      value
    }.headOption
  }
}
