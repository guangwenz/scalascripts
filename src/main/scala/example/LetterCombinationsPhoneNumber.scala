package example

/** Letter Combinations of a Phone Number
  *
  * Given a string containing digits from 2-9 inclusive, return all possible letter combinations that the number could represent. Return the answer in any order.
  *
  * A mapping of digit to letters (just like on the telephone buttons) is given below. Note that 1 does not map to any letters.
  *
  * Constraints:
  *
  * 0 <= digits.length <= 4
  * digits[i] is a digit in the range ['2', '9'].
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/109/backtracking/793/
  */
trait LetterCombinationsPhoneNumber {
  object Solution {
    val digitChars = Map(
      '2' -> ('a' until 'd').toList,
      '3' -> ('d' until 'g').toList,
      '4' -> ('g' until 'j').toList,
      '5' -> ('j' until 'm').toList,
      '6' -> ('m' until 'p').toList,
      '7' -> ('p' until 't').toList,
      '8' -> ('t' until 'w').toList,
      '9' -> ('w' to 'z').toList
    )

    def letterCombinations(digits: String): List[String] = {
      digits.toList match {
        case Nil => Nil
        case head :: Nil =>
          digitChars.get(head).toList.flatten.map(_.toString())
        case head :: tail =>
          val (l, r) = (
            digitChars.get(head).toList.flatten.map(_.toString),
            letterCombinations(tail.mkString)
          )
          (l, r) match {
            case (Nil, Nil)    => Nil
            case (Nil, _ :: _) => r
            case (_ :: _, Nil) => l
            case (_ :: _, _ :: _) =>
              for {
                li <- l
                ri <- r
              } yield li + ri
          }
      }
    }

    def letterCombinations2(digits: String): List[String] = {
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
    println(Solution.letterCombinations("4157063525"))
    println(Solution.letterCombinations("1023"))
    println(Solution.letterCombinations("23"))
    println(Solution.letterCombinations("234"))
    // println(Solution.letterCombinations("134"))
    // println(Solution.letterCombinations("34"))
    // println(Solution.letterCombinations(""))
  }
}
