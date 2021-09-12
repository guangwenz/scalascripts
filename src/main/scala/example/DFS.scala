package example

/** DFS a graph
  */
trait DFS {
  type G[A] = Map[A, List[A]]

  def dfs[A](g: G[A]) = {

    /** recurisive
      * use map as visited should make the contains check constant instead of n
      */
    def dfs0(v: A, visited: List[A]): List[A] = {
      if (visited.contains(v)) visited
      else {
        g(v).foldLeft(v :: visited) { case (s, v) =>
          dfs0(v, s)
        }
      }
    }

    dfs0(g.head._1, List.empty[A]).reverse
  }

  def run() = {
    val g: G[Int] = Map(
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

    println(dfs(g))
  }
}
