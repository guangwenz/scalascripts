package example

/** Letter Combinations of a Phone Number
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/109/backtracking/793/
  */
trait LetterCombinationsPhoneNumber {
  object Solution {
    val digitChars = Map(
      '2' -> ('a' until 'd'),
      '3' -> ('d' until 'g'),
      '4' -> ('g' until 'j'),
      '5' -> ('j' until 'm'),
      '6' -> ('m' until 'p'),
      '7' -> ('p' until 't'),
      '8' -> ('t' until 'w'),
      '9' -> ('w' to 'z')
    )
    def letterCombinations(digits: String): List[String] = {
      digits.toList match {
        case Nil => List()
        case head :: Nil =>
          digitChars.get(head).getOrElse(Nil).map(_.toString()).toList
        case head :: tail =>
          for {
            a <- letterCombinations(tail.mkString)
            b <- digitChars.get(head).getOrElse(Nil)
          } yield b.toString + a
      }
    }
  }
  def run() = {
    println(Solution.letterCombinations("23"))
    println(Solution.letterCombinations("234"))
    println(Solution.letterCombinations(""))
  }
}
