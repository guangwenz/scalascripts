package example

trait LongestSubSequence {

  /** DB table
    */
  def solution2(a: String, b: String): String = {
    val data =
      scala.collection.mutable.ListBuffer.fill(a.size + 1, b.size + 1)("")
    var ret = ""
    for {
      x <- Range(0, a.size + 1) if x > 0
      y <- Range(0, b.size + 1) if y > 0
      _ = {
        if (a(x - 1) == b(y - 1)) data(x)(y) = data(x - 1)(y - 1) + a(x - 1)
        else {
          val last = data(x - 1)(y - 1)
          val (l, t) = (data(x)(y - 1), data(x - 1)(y))
          data(x)(y) = List(last, l, t).sortBy(_.size).last
        }

        if (data(x)(y).size > ret.size) ret = data(x)(y)
      }
    } yield ()
    val output = data
      .map {
        _.collect {
          case "" => "-"
          case f  => f
        }.mkString(",")
      }
      .mkString("\n")
    println(output)
    ret
  }

  /** recursive
    */
  def solution1(a: String, b: String): String = {
    def loop(a: String, b: String, state: String): String = {
      if (a.isEmpty() || b.isEmpty()) state
      else if (a.head == b.head) loop(a.tail, b.tail, state + a.head)
      else {
        val (l, r) = (loop(a.tail, b, state), loop(a, b.tail, state))
        if (l.size > r.size) l else r
      }
    }
    loop(a, b, "")
  }

  def run(): Unit = {
    val testCases = Map(
      ("ABCDGH", "AEDFHR") -> "ADH",
      ("AGGTAB", "GXTXAYB") -> "GTAB"
    )
    for {
      ((a, b), exp) <- testCases
      ret = solution2(a, b)
      _ = {
        if (ret != exp)
          println(s"Failed for input ($a,$b), expected $exp but got $ret")
        else println("Success")
      }
    } yield ()
  }
}
