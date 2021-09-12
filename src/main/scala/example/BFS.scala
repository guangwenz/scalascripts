package example
import scala.collection.mutable

/** traverse a graph with BFS
  */
trait BFS {
  type Graph[A] = Map[A, List[A]]

  def bfs[A](g: Graph[A]): List[A] = {
    // val queue = mutable.seq
    ???
  }
  def run(): Unit = {
    val g: Graph[Int] = Map(
      5 -> List(3, 8),
      3 -> List(2, 4),
      2 -> List(),
      4 -> List(),
      13 -> List(),
      12 -> List(),
      6 -> List(),
      8 -> List(11, 12, 6),
      11 -> List(13)
    )
    println(bfs(g).mkString(","))
  }
}
