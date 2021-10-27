package medium

/** Best Time to Buy and Sell Stock with Transaction Fee
  *
  * You are given an array prices where prices[i] is the price of a given stock on the ith day, and an integer fee representing a transaction fee.
  *
  * Find the maximum profit you can achieve. You may complete as many transactions as you like, but you need to pay the transaction fee for each transaction.
  *
  * Note: You may not engage in multiple transactions simultaneously (i.e., you must sell the stock before you buy again).
  *
  * Constraints:
  *
  * 1 <= prices.length <= 5 * 10^4
  * 1 <= prices[i] < 5 * 10^4
  * 0 <= fee < 5 * 10^4
  *
  * https://leetcode.com/problems/best-time-to-buy-and-sell-stock-with-transaction-fee/
  */
trait BestTimetoBuyandSellStockwithTransactionFee {
  object Solution {
    def maxProfit(prices: Array[Int], fee: Int): Int = {
      ???
    }
  }

  def run() = {
    println(Solution.maxProfit(Array(1, 3, 2, 8, 4, 9), 2) == 8)
    println(Solution.maxProfit(Array(1, 3, 7, 5, 10, 3), 3) == 6)
  }
}
