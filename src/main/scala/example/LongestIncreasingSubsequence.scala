package example

trait LongestIncreasingSubsequence {

  /** recursive solution
    */
  def solution1(input: List[Int]): Int = {
    var globalMax = 1

    def inside(data: List[Int]): Int = {
      if (data.size == 1) 1
      else {
        var localMax = 1
        for {
          i <- Range(1, data.size)
          j = solution1(data.take(i))
        } yield {
          if (data(i - 1) < data.init.last && (j + 1) > localMax) {
            localMax = j + 1
          }
        }
        if (globalMax < localMax) globalMax = localMax
        localMax
      }
    }

    inside(data = input)
    globalMax
  }
  def run(): Unit = {
    val testCases = Map(
      // List(3, 10, 2, 1, 20) -> Set(List(3, 10, 20)),
      // List(2, 7, 4, 3, 8) -> Set(List(2, 7, 8)),
      List(2, 4, 3, 7, 4, 5) -> 4,
      // List(3, 2) -> Set(List(3), List(2)),
      // List(50, 3, 10, 7, 40, 80) -> Set(
      //   List(3, 7, 40, 80),
      //   List(3, 10, 40, 80)
      // ),
      List(3, 10, 7) -> 2
    )
    for {
      (i, exp) <- testCases
      ret = solution1(i)
      _ = {
        if (ret != exp)
          println(s"Failed for input $i, expected $exp but got $ret")
        else println("Success")
      }
    } yield ()
  }
}
