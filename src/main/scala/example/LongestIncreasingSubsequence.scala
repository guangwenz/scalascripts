package example

trait LongestIncreasingSubsequence {

  /** recursive solution
    */
  def solution1(input: List[Int]): Set[List[Int]] = {
    def loop(input: List[Int], state: Set[List[Int]]):Set[List[Int]] = {
        input match {
            case Nil => state
                case head::next = >
        } yieldret
    }
    ???
  }
  def run(): Unit = {
    val testCases = Map(
      List(3, 10, 2, 1, 20) -> Set(List(3, 10, 20)),
      List(3, 2) -> Set(List(3), List(2)),
      List(50, 3, 10, 7, 40, 80) -> Set(List(3, 7, 40, 80))
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
