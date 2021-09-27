package example

/** Best Time to Buy and Sell Stock
  *
  * You are given an array prices where prices[i] is the price of a given stock on the ith day.
  *
  * You want to maximize your profit by choosing a single day to buy one stock and choosing a different day in the future to sell that stock.
  *
  * Return the maximum profit you can achieve from this transaction. If you cannot achieve any profit, return 0.
  * Constraints:
  *
  * 1 <= prices.length <= 105
  * 0 <= prices[i] <= 104
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-easy/97/dynamic-programming/572/
  */
trait BestTimeToBuyAndSellStock {
  object Solution {
    def maxProfit(prices: Array[Int]): Int = {
      prices.tail
        .foldLeft((prices.head, 0)) { case ((lastMin, lastMax), p) =>
          val newMin = if (p < lastMin) p else lastMin
          val newLastMax = {
            val profit = p - lastMin
            if (profit > lastMax) profit
            else lastMax
          }
          (newMin, newLastMax)
        }
        ._2
    }
  }

  def run() = {
    println(Solution.maxProfit(Array(7, 1, 5, 3, 6, 4)) == 5)
    println(Solution.maxProfit(Array(7, 6, 4, 3, 1)) == 0)
  }
}
