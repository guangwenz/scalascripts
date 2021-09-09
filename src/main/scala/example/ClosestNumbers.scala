package example

/** https://www.hackerrank.com/challenges/closest-numbers/problem
  */
trait ClosestNumbers {

  def closestNumbers(arr: Array[Int]): Array[Int] = {
    // Write your code here
    var lastDiff = Int.MaxValue

    val sorted = arr.sorted
    sorted.zipWithIndex.foldLeft(Array.empty[Int]) { case (s, (v, i)) =>
      if (i > 0) {
        val diff = math.abs(sorted(i) - sorted(i - 1))
        if (diff == lastDiff) {
          s ++ Array(sorted(i - 1), sorted(i))
        } else if (diff < lastDiff) {
          lastDiff = diff
          Array(sorted(i - 1), sorted(i))
        } else s
      } else s
    }
  }

  def run() = {
    val ans = Array(-6845551, -6845550, -2845864, -2845863, -1852303, -1852302,
      -643201, -643200, 4338772, 4338773, 6458841, 6458842, 8883985, 8883986)
    for {
      l <- io.Source
        .fromResource("closest_numbers/input1.txt")
        .getLines()
        .toList
    } yield {
      val i = l.split(" ").map(_.toInt)
      val ret = closestNumbers(i)
      println(ret.mkString(" "))
    }
  }
}
