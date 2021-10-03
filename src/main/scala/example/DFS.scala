package example

/** DFS a graph
  */
trait DFS {
  type Graph[A] = Map[A, List[A]]

  /** get all possible paths from vertex A to vertex B
    */
  def paths[A](source: A, target: A, g: Graph[A]): List[List[A]] = ???

  /** is there any cycle in this graph?
    */
  def detectCycle[A](g: Graph[A]): Boolean = ???

  /** is vertex A and B connected in graph?
    */
  def isConnected[A](source: A, target: A, g: Graph[A]): Boolean = {
    val S = collection.mutable.Stack(source)
    val marked = collection.mutable.Map.empty[A, Boolean]
    while (S.nonEmpty) {
      val current = S.pop()
      if (current == target) return true
      marked.put(current, true)
      for {
        adj <- g(current)
        if !marked.contains(adj)
      } yield S.push(adj)
    }
    false
  }

  /** DFS iteratively with stack
    */
  def dfsIter[A](start: A, g: Graph[A]): Unit = {
    val S = collection.mutable.Stack(start)
    val marked = collection.mutable.Map.empty[A, Boolean]
    while (S.nonEmpty) {
      val current = S.pop()
      println(s"visiting $current")
      marked.put(current, true)
      for {
        adj <- g(current)
        if !marked.contains(adj)
      } yield S.push(adj)
    }
  }
  def dfs[A](g: Graph[A]) = {

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

    // println(dfsIter(5, g))
    println(isConnected(2, 6, g) == false)
    println(isConnected(5, 13, g) == true)
    println(isConnected(5, 2, g) == true)
    println(isConnected(12, 6, g) == false)
  }
}
