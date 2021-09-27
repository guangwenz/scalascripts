package example

/** Word Ladder
  *
  * A transformation sequence from word beginWord to word endWord using a dictionary wordList is a sequence of words beginWord -> s1 -> s2 -> ... -> sk such that:
  *
  * Every adjacent pair of words differs by a single letter.
  * Every si for 1 <= i <= k is in wordList. Note that beginWord does not need to be in wordList.
  * sk == endWord
  * Given two words, beginWord and endWord, and a dictionary wordList, return the number of words in the shortest transformation sequence from beginWord to endWord, or 0 if no such sequence exists.
  *
  * Constraints:
  *
  * 1 <= beginWord.length <= 10
  * endWord.length == beginWord.length
  * 1 <= wordList.length <= 5000
  * wordList[i].length == beginWord.length
  * beginWord, endWord, and wordList[i] consist of lowercase English letters.
  * beginWord != endWord
  * All the words in wordList are unique.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/118/trees-and-graphs/842/
  */
trait WordLadder {
  object Solution {
    def ladderLength(
        beginWord: String,
        endWord: String,
        wordList: List[String]
    ): Int = {
      val wordSet = collection.mutable.Set(wordList: _*)
      if (!wordSet.contains(endWord)) return 0

      val q = collection.mutable.Queue(beginWord)
      var res = 0
      while (q.nonEmpty) {
        for {
          k <- q.indices
          word = q.dequeue
          _ = if (word == endWord) return res + 1
          i <- word.indices
          newWord = word.toCharArray()
          ch <- 'a' to 'z'
          _ = {
            newWord(i) = ch
            val s = newWord.mkString
            if (wordSet.contains(s) && s != word) {
              q.enqueue(s)
              wordSet.remove(s)
            }

          }
        } yield ()
        res += 1
      }
      return 0
    }
  }
  object Solution2 {
    def ladderLength(
        beginWord: String,
        endWord: String,
        wordList: List[String]
    ): Int = {
      def buildGraph: Map[String, List[String]] = {
        val g = collection.mutable.Map.empty[String, List[String]]
        for {
          wi <- 0 until wordList.length
          ni <- 0 until wordList.length
          if wi != ni
          w = wordList(wi)
          n = wordList(ni)
          if oneStep(w, n)
        } yield {
          g.get(w) match {
            case None    => g.put(w, List(n))
            case Some(v) => g.put(w, n +: v)
          }
        }
        g.toMap
      }

      def oneStep(beginWord: String, endWord: String): Boolean = {
        (0 until beginWord.length()).foldLeft(0) { case (s, i) =>
          if (beginWord(i) == endWord(i)) s else s + 1
        } == 1
      }

      def shortestPath(start: String, g: Map[String, List[String]]): Int = {
        def loop(start: String, visited: List[String]): Int = {
          if (start == endWord) 1
          else {
            val p = for {
              ad <- g.get(start).getOrElse(Nil)
              if !visited.contains(ad)
              x = {
                loop(ad, ad +: visited)
              }
              if x > -1
            } yield {
              x + 1
            }
            val ret =
              if (p.isEmpty) -1
              else p.min
            println(
              s"ret is $ret with current $start and children ${g
                .get(start)} visited $visited $p"
            )
            ret
          }
        }
        val ret = loop(start, Nil)
        if (ret <= 0) 0 else ret
      }

      val g = buildGraph
      println(g)
      val candidates = wordList
        .filter(w => w == beginWord)
        .map(shortestPath(_, g))
        .filter(_ != 0) :::
        wordList
          .filter(oneStep(beginWord, _))
          .map(shortestPath(_, g))
          .filter(_ != 0)
          .map(_ + 1)

      if (candidates.isEmpty) 0
      else candidates.min
    }
  }

  def run() = {
    println(
      Solution.ladderLength(
        "hot",
        "dog",
        List("hot", "cog", "dog", "tot", "hog", "hop", "pot", "dot")
      ) == 3
    )
    println(
      Solution.ladderLength(
        "hit",
        "cog",
        List("hot", "dot", "dog", "lot", "log", "cog")
      ) == 5
    )
    println(
      Solution.ladderLength(
        "hit",
        "cog",
        List("hot", "dot", "dog", "lot", "log")
      ) == 0
    )
    println(
      Solution.ladderLength(
        "hot",
        "dog",
        List("hot", "dog")
      ) == 0
    )
    println(
      Solution.ladderLength(
        "qa",
        "sq",
        List(
          "si",
          "go",
          "se",
          "cm",
          "so",
          "ph",
          "mt",
          "db",
          "mb",
          "sb",
          "kr",
          "ln",
          "tm",
          "le",
          "av",
          "sm",
          "ar",
          "ci",
          "ca",
          "br",
          "ti",
          "ba",
          "to",
          "ra",
          "fa",
          "yo",
          "ow",
          "sn",
          "ya",
          "cr",
          "po",
          "fe",
          "ho",
          "ma",
          "re",
          "or",
          "rn",
          "au",
          "ur",
          "rh",
          "sr",
          "tc",
          "lt",
          "lo",
          "as",
          "fr",
          "nb",
          "yb",
          "if",
          "pb",
          "ge",
          "th",
          "pm",
          "rb",
          "sh",
          "co",
          "ga",
          "li",
          "ha",
          "hz",
          "no",
          "bi",
          "di",
          "hi",
          "qa",
          "pi",
          "os",
          "uh",
          "wm",
          "an",
          "me",
          "mo",
          "na",
          "la",
          "st",
          "er",
          "sc",
          "ne",
          "mn",
          "mi",
          "am",
          "ex",
          "pt",
          "io",
          "be",
          "fm",
          "ta",
          "tb",
          "ni",
          "mr",
          "pa",
          "he",
          "lr",
          "sq",
          "ye"
        )
      ) == 5
    )
  }
}
