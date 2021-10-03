package example
import scala.collection.mutable

/** traverse a graph with BFS
  */
trait BFS {
  type Graph[A] = Map[A, List[A]]

  /** calculate each node's level from start point inside graph
    */
  def levels[A](start: A, g: Graph[A]): Map[A, Int] = {
    val Q = collection.mutable.Queue(start)
    val levels = collection.mutable.Map(start -> 0)
    val marked = collection.mutable.Map.empty[A, Boolean]
    while (Q.nonEmpty) {
      val current = Q.dequeue
      marked.put(current, true)
      for {
        adj <- g(current)
        if !marked.contains(adj)
      } yield {
        levels.put(adj, levels(current) + 1)
        Q.enqueue(adj)
      }
    }
    levels.toMap
  }

  def bfs[A](start: A, g: Graph[A]): Unit = {
    val Q = collection.mutable.Queue(start)
    val marked = collection.mutable.Map.empty[A, Boolean]
    while (Q.nonEmpty) {
      val current = Q.dequeue()
      println(s"visiting $current")
      marked.put(current, true)
      for {
        adj <- g(current)
        if !marked.contains(adj)
      } yield {
        Q.enqueue(adj)
      }
    }
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
    println(levels(5, g))
  }
}
