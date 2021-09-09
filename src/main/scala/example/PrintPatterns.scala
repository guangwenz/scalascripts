package example

trait PrintPatterns {
  def snake(input: Seq[Seq[Int]]) = {
    var switch = false
    for {
      r <- input
      _ = {
        switch = !switch
      }
      c <- {
        println(switch)
        if (switch) Range(r.size - 1, -1, -1) else Range(0, r.size)
      }

    } yield {
      println(c)
      // println(r(c))
    }
  }
  def boundry(in: Seq[Seq[Int]]): Unit = {
    for {
      first <- in.headOption
      rightL <- Some(in.tail.map(_.last))
      bottom <- in.lastOption.map(_.init.tail.reverse)
      left <- Some(in.map(_.head).reverse.init)
    } yield {
      println((first ++ rightL ++ bottom ++ left).mkString(","))
    }
  }
  def run() = {
    val input = Seq(
      Seq(1, 2, 3, 4),
      Seq(5, 6, 7, 8),
      Seq(9, 10, 11, 12),
      Seq(13, 14, 15, 16),
      Seq(17, 18, 19, 20)
    )
    // snake(input)
    boundry(input)
  }
}
