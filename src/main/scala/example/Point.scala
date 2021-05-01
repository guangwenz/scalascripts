package example

trait Point {
  def _do(input: List[Int], lastSum: Int, lastRet: String): String = {
    input match {
      case head :: Nil =>
        Point.Ops.all
          .map {
            case Point.Ops.Add =>
              if (head + lastSum == 24) lastRet + s"+$head" else ""
            case Point.Ops.Minus =>
              if (lastSum - head == 24) lastRet + s"-$head" else ""
            case Point.Ops.Multi =>
              if (lastSum * head == 24) lastRet + s"*$head" else ""
            case Point.Ops.Div =>
              if (lastSum / head == 24) lastRet + s"/$head" else ""
            case _ => ""
          }
          .collect { case s if s.nonEmpty => s }
          .distinct
          .mkString(" ")

      case head :: next =>
        Point.Ops.all
          .map {
            case Point.Ops.Add =>
              val op = if (lastRet.nonEmpty) "+" else ""
              _do(next, lastSum + head, lastRet + op + s"$head")
            case Point.Ops.Minus =>
              val op = if (lastRet.nonEmpty) "-" else ""
              _do(next, lastSum - head, lastRet + op + s"$head")
            case Point.Ops.Multi =>
              val op = if (lastRet.nonEmpty) "*" else ""
              if (lastSum == 0) _do(next, head, lastRet + op + s"$head")
              else _do(next, lastSum * head, lastRet + op + s"$head")
            case Point.Ops.Div =>
              val op = if (lastRet.nonEmpty) "/" else ""
              if (lastSum == 0) _do(next, head, lastRet + op + s"$head")
              else _do(next, lastSum / head, lastRet + op + s"$head")

            case _ => ""
          }
          .collect { case s if s.nonEmpty => s }
          .distinct
          .mkString(" ")
      case _ => ""

    }
  }

  val testCases = Map(
    List(6, 6, 6, 6) -> "6+6+6+6 6*6-6-6",
    List(2, 10, 7, 5) -> "2+10+7+5",
    List(3, 9, 8, 4) -> "3+9+8+4 3+9*8/4",
    List(4, 8, 1, 11) -> "4+8+1+11",
    List(18, 1, 3, 4) -> "18+1/3*4 18-1+3+4 18*1/3*4 18/1/3*4",
    List(24, 1, 2, 1) -> "24+1-2+1 24-1+2-1",
    List(4, 6, 1, 1) -> "4*6+1-1 4*6-1+1 4*6*1*1 4*6*1/1 4*6/1*1 4*6/1/1"
  )

  def run(): Unit = {
    testCases.foreach { case (input, expectedRet) =>
      val ret = _do(input, 0, "")
      if (ret != expectedRet) {
        println(s"Failed ${input.mkString(",")}, got $ret")
      } else {
        println(s"Success, ${input.mkString(",")} = $ret")
      }
    }
  }
}

object Point {
  sealed trait Op
  object Ops {
    case object Add extends Op
    case object Minus extends Op
    case object Multi extends Op
    case object Div extends Op

    val all = List(Add, Minus, Multi, Div)
    val addMinus = List(Add, Minus)
  }
}
