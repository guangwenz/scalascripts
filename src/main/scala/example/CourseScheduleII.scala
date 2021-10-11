package example

/** Course Schedule II
  *
  * There are a total of numCourses courses you have to take, labeled from 0 to numCourses - 1. You are given an array prerequisites where prerequisites[i] = [ai, bi] indicates that you must take course bi first if you want to take course ai.
  *
  * For example, the pair [0, 1], indicates that to take course 0 you have to first take course 1.
  * Return the ordering of courses you should take to finish all courses. If there are many valid answers, return any of them. If it is impossible to finish all courses, return an empty array.
  *
  * Constraints:
  *
  * 1 <= numCourses <= 2000
  * 0 <= prerequisites.length <= numCourses * (numCourses - 1)
  * prerequisites[i].length == 2
  * 0 <= ai, bi < numCourses
  * ai != bi
  * All the pairs [ai, bi] are distinct.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/118/trees-and-graphs/848/
  */
trait CourseScheduleII {
  object Solution {
    def findOrder(
        numCourses: Int,
        prerequisites: Array[Array[Int]]
    ): Array[Int] = {

      //build graph
      val g = collection.mutable.Map.empty[Int, List[Int]]
      for {
        row <- 0 until prerequisites.length
        courseI <- prerequisites(row).length - 2 to 0 by -1
        course = prerequisites(row)(courseI)
        courseDep = prerequisites(row)(courseI + 1)
      } yield {
        g.put(course, g.get(course).fold(List(courseDep))(courseDep +: _))
      }

      val indCourses = List.range(0, numCourses).filter(!g.contains(_))
      // println(g)
      //BFS
      def visit(startCourse: Int): List[Int] = {
        val visited = collection.mutable.Map.empty[Int, Boolean]
        val courseLevel = collection.mutable.Map(startCourse -> 0)
        val courseOrder = collection.mutable.Map(0 -> List(startCourse))

        val S = collection.mutable.Stack(startCourse)
        while (S.nonEmpty) {
          val current = S.pop()
          visited.put(current, true)
          for {
            adj <- g.get(current).getOrElse(Nil)
            if !visited.contains(adj)
          } yield {
            courseLevel.put(adj, courseLevel(current) + 1)
            val nextLevel = courseLevel(current) + 1
            courseOrder.put(
              nextLevel,
              adj +: courseOrder.get(nextLevel).getOrElse(Nil)
            )
            S.push(adj)
          }
        }
        val depCourses =
          courseOrder.keys.toList.sortBy(i => -i).foldLeft(List.empty[Int]) {
            case (s, i) => courseOrder(i) ::: s
          }
        depCourses
      }
      val p = for {
        (c, _) <- g
        ret = {
          visit(c)
        }
      } yield ret
      val a = p.toList.sortBy(-_.length).headOption.getOrElse(Nil)
      val ret =
        a.reverse ::: indCourses
          .filterNot(a.contains(_))
      // println(ret)
      ret.toArray
    }
  }

  def run() = {
    println(Solution.findOrder(2, Array(Array(1, 0))).sameElements(Array(0, 1)))
    println(
      Solution
        .findOrder(2, Array(Array(0, 1), Array(1, 0)))
        .isEmpty
    )
    println(Solution.findOrder(1, Array()).sameElements(Array(0)))
    println(
      Solution
        .findOrder(4, Array(Array(1, 0), Array(2, 0), Array(3, 1), Array(3, 2)))
        .sameElements(Array(0, 2, 1, 3))
    )
  }
}
