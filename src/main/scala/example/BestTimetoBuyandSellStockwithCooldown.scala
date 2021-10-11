package example

/** Best Time to Buy and Sell Stock with Cooldown
  *
  * You are given an array prices where prices[i] is the price of a given stock on the ith day.
  *
  * Find the maximum profit you can achieve. You may complete as many transactions as you like (i.e., buy one and sell one share of the stock multiple times) with the following restrictions:
  *
  * After you sell your stock, you cannot buy stock on the next day (i.e., cooldown one day).
  * Note: You may not engage in multiple transactions simultaneously (i.e., you must sell the stock before you buy again).
  *
  * Constraints:
  *
  * 1 <= prices.length <= 5000
  * 0 <= prices[i] <= 1000
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/121/dynamic-programming/862/
  */
trait BestTimetoBuyandSellStockwithCooldown {
  object Solution {
    def maxProfit(prices: Array[Int]): Int = {
      val memo = collection.mutable.Map.empty[List[Int], Int]
      def inner(prices: List[Int]): Int = {
        if (memo.contains(prices)) memo(prices)
        else {
          val ret = prices match {
            case Nil           => 0
            case _ :: Nil      => 0
            case b :: s :: Nil => math.max(s - b, 0)
            case _ =>
              val p = for {
                b <- 0 until prices.length - 1
                nobuy = inner(prices.slice(b + 1, prices.length))
                s <- b + 1 until prices.length
                profit = prices(s) - prices(b)
                restProfit = inner(prices.slice(s + 2, prices.length))
              } yield math.max(profit + restProfit, nobuy)
              p.max
          }
          memo.put(prices, ret)
          ret
        }
      }
      inner(prices.toList)
    }
  }

  def run() = {
    println(Solution.maxProfit(Array(1, 2, 3, 0, 2)) == 3)
    println(Solution.maxProfit(Array(4, 2, 1)) == 0)
    println(Solution.maxProfit(Array(2, 1)) == 0)
    println(Solution.maxProfit(Array(1)) == 0)
    println(
      Solution.maxProfit(
        Array(48, 12, 60, 93, 97, 42, 25, 64, 17, 56, 85, 93, 9, 48, 52, 42, 58,
          85, 81, 84, 69, 36, 1, 54, 23, 15, 72, 15, 11, 94)
      ) == 428
    )
  }
}
