package easy

/** First Bad Version
  *
  * You are a product manager and currently leading a team to develop a new product. Unfortunately, the latest version of your product fails the quality check. Since each version is developed based on the previous version, all the versions after a bad version are also bad.
  *
  * Suppose you have n versions [1, 2, ..., n] and you want to find out the first bad one, which causes all the following ones to be bad.
  *
  * You are given an API bool isBadVersion(version) which returns whether version is bad. Implement a function to find the first bad version. You should minimize the number of calls to the API.
  *
  * Constraints:
  *
  * 1 <= bad <= n <= 2^31 - 1
  *
  * https://leetcode.com/problems/first-bad-version/
  */
trait FirstBadVersion {
  trait VersionControl {
    def isBadVersion(version: Int): Boolean = version >= 1702766719
  }
  /* The isBadVersion API is defined in the parent class VersionControl.
      def isBadVersion(version: Int): Boolean = {} */
  class Solution extends VersionControl {
    def firstBadVersion(n: Int): Int = {
      @annotation.tailrec
      def inner(start: Int, end: Int): Int = {
        if (start > end) -1
        else {
          val m = start + (end - start) / 2
          if (isBadVersion(m)) {
            if (m - 1 >= 0 && isBadVersion(m - 1))
              inner(start, m - 1)
            else m
          } else {
            inner(m + 1, end)
          }
        }
      }
      inner(1, n)

      //   another solution

      //   var (low, high) = (0, n - 1)
      //   while (low <= high) {
      //     val m = low + (high - low) / 2
      //     if (isBadVersion(m)) {
      //       high = m - 1
      //     } else {
      //       low = m + 1
      //     }
      //   }
      //   low
    }
  }

  def run() = {
    // println(new Solution().firstBadVersion(1) == 1)
    println(new Solution().firstBadVersion(2126753390) == 1702766719)
  }
}
