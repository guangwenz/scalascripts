package example

/** There are a total of numCourses courses you have to take, labeled from 0 to numCourses - 1. You are given an array prerequisites where prerequisites[i] = [ai, bi] indicates that you must take course bi first if you want to take course ai.
  *
  * For example, the pair [0, 1], indicates that to take course 0 you have to first take course 1.
  * Return true if you can finish all courses. Otherwise, return false.
  *
  * Constraints:
  *
  * 1 <= numCourses <= 10^5
  * 0 <= prerequisites.length <= 5000
  * prerequisites[i].length == 2
  * 0 <= ai, bi < numCourses
  * All the pairs prerequisites[i] are unique.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/118/trees-and-graphs/847/
  */
trait CourseSchedule {
  object Solution {
    val WHITE = 0
    val GRAY = 1
    val BLACK = 2

    val ENTER = 1
    val EXIT = 2

    def canFinish(
        numCourses: Int,
        prerequisites: Array[Array[Int]]
    ): Boolean = {
      def isCyclic(graph: Map[Int, Set[Int]]): Boolean = {
        def isC(start: Int): Boolean = {
          val stack = collection.mutable.Stack((ENTER, start))
          val state =
            collection.mutable.Map(graph.keySet.map(_ -> WHITE).toSeq: _*)

          while (stack.nonEmpty) {
            val (act, v) = stack.pop()
            act match {
              case ENTER =>
                state.put(v, GRAY)
                stack.push((EXIT, v))
                for {
                  adj <- graph(v) if state.contains(adj)
                  _ = {
                    if (state(adj) == GRAY) return true
                    else if (state(adj) == WHITE) {
                      stack.push((ENTER, adj))
                    }
                  }
                } yield ()
              case EXIT =>
                state.put(v, BLACK)
            }
          }
          false
        }

        for {
          (v, _) <- graph
          _ = {
            if (isC(v)) return true
          }
        } yield ()
        false
      }

      val g = collection.mutable.Map.empty[Int, Set[Int]]
      for {
        p <- prerequisites
        _ = {
          (0 until p.length - 1).foreach { i =>
            g.get(p(i)) match {
              case None    => g.put(p(i), Set(p(i + 1)))
              case Some(v) => g.put(p(i), v + p(i + 1))
            }
          }
        }
      } yield ()
      !isCyclic(g.toMap)
    }
  }

  def run() = {
    // println(Solution.canFinish(2, Array(Array(1, 0))) == true)
    // println(Solution.canFinish(2, Array(Array(1, 0))) == true)
    println(
      Solution.canFinish(
        5,
        Array(Array(5, 4, 1), Array(3, 4, 1), Array(2))
      ) == true
    )
  }
}
