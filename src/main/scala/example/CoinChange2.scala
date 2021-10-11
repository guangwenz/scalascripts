package example

/** Coin Change 2
  * You are given an integer array coins representing coins of different denominations and an integer amount representing a total amount of money.
  *
  * Return the number of combinations that make up that amount. If that amount of money cannot be made up by any combination of the coins, return 0.
  *
  * You may assume that you have an infinite number of each kind of coin.
  *
  * The answer is guaranteed to fit into a signed 32-bit integer.
  *
  * Constraints:
  *
  * 1 <= coins.length <= 300
  * 1 <= coins[i] <= 5000
  * All the values of coins are unique.
  * 0 <= amount <= 5000
  *
  * https://leetcode.com/problems/coin-change-2/
  */
trait CoinChange2 {
  object Solution {
    def change(amount: Int, coins: Array[Int]): Int = {
      val dp = Array.fill(coins.length + 1, amount + 1)(0)
      dp(0)(0) = 1

      (1 to amount).foreach(i => dp(0)(i) = 0)
      (1 to coins.length).foreach(i => dp(i)(0) = 1)

      for {
        c <- 1 to coins.length
        a <- 1 to amount
      } yield {
        dp(c)(a) =
          if (a < coins(c - 1)) dp(c - 1)(a)
          else dp(c - 1)(a) + dp(c)(a - coins(c - 1))
      }

      dp(coins.length)(amount)
    }
  }

  object Solution2 {
    def change(amount: Int, coins: Array[Int]): Int = {
      val memo = collection.mutable.Map.empty[(Int, List[Int]), Int]
      def inner(amount: Int, coins: List[Int]): Int = {
        if (memo.contains((amount, coins.toList))) memo((amount, coins.toList))
        else {
          val ret = amount match {
            case 0          => 1
            case x if x < 0 => 0
            case x =>
              coins match {
                case Nil                         => 0
                case a :: Nil if amount % a == 0 => 1
                case a :: tail =>
                  inner(amount - a, coins) + inner(amount, tail)
              }
          }
          memo.put((amount, coins.toList), ret)
          ret
        }
      }
      inner(amount, coins.toList)
    }
  }

  def run() = {
    println(Solution.change(4, Array(1, 2)) == 3)
    println(Solution.change(0, Array(7)) == 1)
    println(Solution.change(5, Array(1, 2, 5)) == 4)
    println(Solution.change(3, Array(2)) == 0)
    println(Solution.change(10, Array(10)) == 1)
    println(Solution.change(500, Array(3, 5, 7, 8, 9, 10, 11)) == 35502874)
  }
}
