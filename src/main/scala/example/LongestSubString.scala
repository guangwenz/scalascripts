/*
 * Copyright (c) 2019 Crunchbase - All Rights Reserved.
 * Unauthorized copying of this file, via any medium is strictly prohibited.
 * This file is proprietary and confidential.
 * Last modified by guangwenz at (see git)
 */
package example

trait LongestSubString {

  /** DP table
    */
  def solution3(a: String, b: String): String = {
    val data =
      scala.collection.mutable.ListBuffer.fill(a.size + 1, b.size + 1)("")
    val (m, n) = (a.size + 1, b.size + 1)
    var result = ""
    for {
      mi <- Range(0, m) if mi > 0
      ni <- Range(0, n) if ni > 0
      _ = {
        if (a(mi - 1) == b(ni - 1)) {
          data(mi)(ni) = data(mi - 1)(ni - 1) + a(mi - 1)
          if (data(mi)(ni).size > result.size) result = data(mi)(ni)
        } else data(mi)(ni) = ""
      }
    } yield ()
    result
  }

  /** recursive
    */
  def solution2(a: String, b: String): String = {
    def loop(a: String, b: String, ai: Int, bi: Int, state: String): String = {
      if (ai < 0 || bi < 0) return state
      if (a(ai) == b(bi)) {
        loop(a, b, ai - 1, bi - 1, a(ai) + state)
      } else {
        val left = loop(a, b, ai - 1, bi, "")
        val right = loop(a, b, ai, bi - 1, "")
        List(state, left, right).sortBy(_.size).last
      }
    }
    loop(a, b, a.size - 1, b.size - 1, "")
  }

  /** brutal force
    * check every possible substring of short one and checc if it's substring of longer one
    */
  def solution1(a: String, b: String): Set[String] = {

    val (x, y) = if (a.size > b.size) (b, a) else (a, b)
    var subs = List.empty[String]
    var lastMax = 0

    for {
      (m, mi) <- x.zipWithIndex
      (n, ni) <- x.substring(mi).zipWithIndex
      ret = {
        val sub = x.substring(mi, mi + ni + 1)
        if (y.contains(sub)) {
          if (sub.size > lastMax) {
            subs = List(sub)
            lastMax = sub.size
          }
          if (sub.size == lastMax) subs = sub +: subs

        }
      }
    } yield ()
    subs.distinct.toSet
  }

  def run(): Unit = {
    val testCases = Map(
      // ("abc", "c") -> "c"
      ("abcd", "cdef") -> "cd",
      ("GeeksforGeeks", "GeeksQuiz") -> "Geeks",
      ("abcdxyz", "xyzabcd") -> "abcd",
      ("zxabcdezy", "yzabcdezx") -> "abcdez",
      ("ABABC", "BABCA") -> "BABC",
      ("ABCBA", "ABABC") -> "ABC",
      ("ABCDGH", "ACDGHRX") -> "CDGH",
      ("ABC", "AC") -> "A",
      ("A", "ABABC") -> "A",
      ("CDE", "A") -> "",
      ("ABCBA", "") -> ""
    )
    for {
      ((a, b), exp) <- testCases
      ret = {
        solution3(a, b)
      }
    } yield {
      if (ret != exp)
        println(s"FAILED for input $a and $b, expected $exp but got $ret")
    }
  }
}
