package example

/** You are given an integer array coins representing coins of different denominations and an integer amount representing a total amount of money.
  * Return the fewest number of coins that you need to make up that amount. If that amount of money cannot be made up by any combination of the coins, return -1.
  * You may assume that you have an infinite number of each kind of coin.
  *
  * Constraints:
  *  1 <= coins.length <= 12
  *  1 <= coins[i] <= 2^31 - 1
  *  0 <= amount <= 10^4
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/111/dynamic-programming/809/
  */

trait CoinChange {

  object Solution {
    def coinChange(coins: Array[Int], amount: Int): Int = {
      val memo = collection.mutable.Map.empty[Int, Int]
      def loop(coins: Array[Int], amount: Int): Int =
        amount match {
          case 0          => 0
          case x if x < 0 => -1
          case _ =>
            memo
              .get(amount)
              .fold {
                val ret = coins
                  .map { c => loop(coins, amount - c) }
                  .collect {
                    case x if x >= 0 => x + 1
                  }
                  .sorted
                  .headOption
                  .getOrElse(-1)
                memo.put(amount, ret)
                ret
              }(i => i)
        }
      loop(coins, amount)
    }
  }

  /** resursive solution
    */
  object Solution2 {
    def coinChange(coins: Array[Int], amount: Int): Int = {
      val memo = collection.mutable.Map.empty[Int, Int]
      def loop(coins: Array[Int], amount: Int): Int = {
        if (amount < 0) -1
        else if (amount == 0) 0
        else {
          memo.get(amount) match {
            case None =>
              val p = for {
                c <- coins
                re = loop(coins, amount - c)
                if re >= 0
              } yield re + 1
              val ret = p.sorted.headOption.getOrElse(-1)
              memo.put(amount, ret)
              ret
            case Some(v) => v
          }
        }
      }
      loop(coins, amount)
    }
  }

  def run() = {
    println(Solution.coinChange(Array(2, 5, 10, 1), 27) == 4)
    println(Solution.coinChange(Array(1, 3, 5), 7) == 3)
    println(Solution.coinChange(Array(1, 2, 5), 11) == 3)
    println(Solution.coinChange(Array(2), 3) == -1)
    println(Solution.coinChange(Array(1), 0) == 0)
    println(Solution.coinChange(Array(1), 1) == 1)
    println(Solution.coinChange(Array(1), 2) == 2)
    println(Solution.coinChange(Array(186, 419, 83, 408), 6249) == 20)
    println(Solution.coinChange(Array(1, 2147483647), 2) == 2)
  }
}
