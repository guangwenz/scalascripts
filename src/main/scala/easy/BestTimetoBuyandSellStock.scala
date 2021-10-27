package easy

/** Best Time to Buy and Sell Stock
  * You are given an array prices where prices[i] is the price of a given stock on the ith day.
  *
  * You want to maximize your profit by choosing a single day to buy one stock and choosing a different day in the future to sell that stock.
  *
  * Return the maximum profit you can achieve from this transaction. If you cannot achieve any profit, return 0.
  *
  * Constraints:
  *
  * 1 <= prices.length <= 10^5
  * 0 <= prices[i] <= 10^4
  */
trait BestTimetoBuyandSellStock {
  object Solution {
    def maxProfit(prices: Array[Int]): Int = {
      var result = 0
      prices.tail.foldLeft(prices.head) { case (lastMin, p) =>
        result = math.max(result, p - lastMin)
        if (lastMin > p) p else lastMin
      }
      result
    }
  }
  def run() = {
    println(Solution.maxProfit(Array(7, 1, 5, 3, 6, 4)) == 5)
    println(Solution.maxProfit(Array(7, 6, 4, 3, 1)) == 0)
    println(Solution.maxProfit(Array(4, 1, 2)) == 1)
    println(Solution.maxProfit(Array(7, 4, 1, 2)) == 1)
  }
}
