/*
 * Copyright (c) 2019 Crunchbase - All Rights Reserved.
 * Unauthorized copying of this file, via any medium is strictly prohibited.
 * This file is proprietary and confidential.
 * Last modified by guangwenz at (see git)
 */
package example

trait EditDistance {

  /** non recursive solution with DP table O(mn)
    */
  def solution2(source: String, target: String): Int = {
    if (source.isEmpty() || target.isEmpty())
      return List(source.size, target.size).max

    val data = scala.collection.mutable.ListBuffer
      .fill(source.size + 1, target.size + 1)(0)
    for {
      mi <- Range(0, source.size + 1) if mi > 0
      ni <- Range(0, target.size + 1) if ni > 0
      _ = {
        if (source(mi - 1) == target(ni - 1))
          data(mi)(ni) = data(mi - 1)(ni - 1)
        else
          data(mi)(ni) = List(
            data(mi - 1)(ni - 1),
            data(mi - 1)(ni),
            data(mi)(ni - 1)
          ).min + 1
      }
    } yield ()
    val output = data.map(_.mkString(",")).mkString("\n")
    println(output)
    data(source.size)(target.size)
  }

  /** recursive solution with expotential time performance
    */
  def solution1(source: String, target: String): Int = {
    def loop(s: String, t: String, state: Int): Int = {
      (s, t) match {
        case (m, n) if m.isEmpty() || n.isEmpty() =>
          state + List(m.size, n.size).sorted.last
        case (m, n) if m.head == n.head => loop(m.tail, n.tail, state)
        case (m, n)                     =>
          //substitution
          val a = loop(m.tail, n.tail, state + 1)
          //remove
          val b = loop(m, n.tail, state + 1)
          //insert
          val c = loop(m.tail, n, state + 1)
          List(a, b, c).sorted.head
      }
    }
    loop(source, target, 0)
  }
  def run(): Unit = {
    val testCases = Map(
      ("kitten", "sitting") -> 3,
      ("survey", "surgery") -> 2,
      ("hello", "") -> 5,
      ("", "world") -> 5,
      ("", "") -> 0
    )
    for {
      ((a, b), exp) <- testCases
      ret = {
        solution2(a, b)
      }
      _ = {
        if (ret != exp)
          println(s"FAILED for input $a and $b, expected $exp but got $ret")
        else println("Success")
      }
    } yield ()
  }
}
