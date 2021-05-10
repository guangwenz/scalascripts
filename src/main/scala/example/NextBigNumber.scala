package example

trait NextBigNumber {

  def swap(in: String, xi: Int, x: Char, yi: Int, y: Char): String = {
    in.zipWithIndex.collect { case (c, i) =>
      if (i == xi) y else if (i == yi) x else c
    }.mkString
  }
  def program(in: String): String = {
    var lastNum = Long.MaxValue
    for ((x, xi) <- in.zipWithIndex) {
      for ((y, yi) <- in.substring(xi).zipWithIndex) {
        val f = swap(in, xi, x, xi + yi, y).toLong
        if (f < lastNum && f > in.toLong) lastNum = f
      }
    }
    if (lastNum == Long.MaxValue) in
    else
      lastNum.toString()
  }

  def run() {

    val testCases = List(
      "1" -> "1",
      "12" -> "21",
      "4312" -> "4321",
      "1111" -> "1111",
      "1234" -> "1243",
      "89875" -> "98875",
      "23456" -> "23465",
      "23153" -> "23351"
    )

    for ((i, r) <- testCases) {
      val c = program(i)
      if (c == r) {
        println("Success")
      } else {
        println(s"Fail! result $c with input $i is not equal to expected $r")
      }
    }
  }
}
