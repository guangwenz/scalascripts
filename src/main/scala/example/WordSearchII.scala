package example

import scala.language.postfixOps

/** Word Search II
  * Given an m x n board of characters and a list of strings words, return all words on the board.
  *
  * Each word must be constructed from letters of sequentially adjacent cells, where adjacent cells are horizontally or vertically neighboring. The same letter cell may not be used more than once in a word.
  *
  * Constraints:
  *
  * m == board.length
  * n == board[i].length
  * 1 <= m, n <= 12
  * board[i][j] is a lowercase English letter.
  * 1 <= words.length <= 3 * 10^4
  * 1 <= words[i].length <= 10
  * words[i] consists of lowercase English letters.
  * All the strings of words are unique.
  *
  * https://leetcode.com/explore/interview/card/top-interview-questions-hard/119/backtracking/853/
  */
trait WordSearchII {

  object Solution {
    case class TrieNode(
        endOfWord: Boolean,
        boardPos: List[(Int, Int)],
        children: Array[Option[TrieNode]]
    )
    object TrieNode {
      def node: TrieNode =
        TrieNode(
          false,
          Nil,
          (0 until 26).map(_ => Option.empty[TrieNode]).toArray
        )

      def insert(root: TrieNode, s: String): Unit = {
        (0 until s.length).foldLeft(root) { case (current, level) =>
          val idx = s(level) - 'a'
          current.children(idx) match {
            case Some(value) => value
            case None =>
              current.children(idx) = {
                val n =
                  if (level == s.length - 1)
                    TrieNode.node.copy(endOfWord = true)
                  else TrieNode.node
                Some(n)
              }
              current.children(idx).get
          }
        }
      }

      def search(root: TrieNode, key: String): Boolean = {
        val (_, word) = (0 until key.length).foldLeft((root, "")) {
          case ((current, w), level) =>
            val idx = key(level) - 'a'
            current.children(idx) match {
              case Some(v) if v.boardPos.nonEmpty => (v, w + key(level))
              case _                              => (current, w)
            }
        }
        word.mkString == key
      }
    }
    def findWords(
        board: Array[Array[Char]],
        words: Array[String]
    ): List[String] = { Nil }
  }

  /** timeout solution without trie
    */
  object Solution2 {
    def findWords(
        board: Array[Array[Char]],
        words: Array[String]
    ): List[String] = {
      def find(word: String): Boolean = {
        def inner(
            start: (Int, Int),
            word: String,
            visited: Map[(Int, Int), Boolean]
        ): Boolean = {
          if (word.isEmpty) true
          else {
            val (r, c) = start
            val adjs = for {
              adj <- List((r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1))
              if {
                val (r, c) = adj
                !visited.contains(
                  adj
                ) && r >= 0 && r < board.length && c >= 0 && c < board.head.length && board(
                  r
                )(c) == word.head
              }
            } yield adj
            if (adjs.isEmpty) false
            else
              adjs.exists(adj =>
                inner(adj, word.tail, visited + (start -> true))
              )
          }

        }
        val p = for {
          r <- board.indices
          c <- board(r).indices
          if board(r)(c) == word.head && inner((r, c), word.tail, Map.empty)
        } yield (r, c)

        p.nonEmpty
      }

      val ret = words.toList.filter(w => find(w))
      ret
    }
  }

  def run() = {
    // println(
    //   Solution
    //     .findWords(
    //       Array(
    //         Array('o', 'a', 'a', 'n'),
    //         Array('e', 't', 'a', 'e'),
    //         Array('i', 'h', 'k', 'r'),
    //         Array('i', 'f', 'l', 'v')
    //       ),
    //       Array("oath", "pea", "eat", "rain")
    //     )
    //     .sameElements(Array("oath", "eat"))
    // )
    // println(
    //   Solution
    //     .findWords(
    //       Array(
    //         Array('a', 'b'),
    //         Array('c', 'd')
    //       ),
    //       Array("abcb")
    //     )
    //     .isEmpty
    // )
    // println(
    //   Solution
    //     .findWords(
    //       Array(
    //         Array('a', 'a')
    //       ),
    //       Array("aaa")
    //     )
    //     .isEmpty
    // )
    // println(
    //   Solution
    //     .findWords(
    //       Array(
    //         Array('a', 'b', 'e'),
    //         Array('b', 'c', 'd')
    //       ),
    //       Array("abcdeb")
    //     )
    //     .sameElements(Array("abcdeb"))
    // )
    // println(
    //   Solution
    //     .findWords(
    //       Array(
    //         Array('a', 'b'),
    //         Array('a', 'a')
    //       ),
    //       Array("aba", "baa", "bab", "aaab", "aaa", "aaaa", "aaba")
    //     )
    //     .sameElements(Array("aba", "baa", "aaab", "aaa", "aaba"))
    // )
    // println(
    //   Solution
    //     .findWords(
    //       Array(
    //         Array('a', 'b', 'c', 'e'),
    //         Array('z', 'z', 'd', 'z'),
    //         Array('z', 'z', 'c', 'z'),
    //         Array('z', 'a', 'b', 'z')
    //       ),
    //       Array("abcdce")
    //     )
    //     .sameElements(Array("abcdce"))
    // )
    println(
      Solution
        .findWords(
          Array(
            Array('b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a'),
            Array('a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b'),
            Array('b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a'),
            Array('a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b'),
            Array('b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a'),
            Array('a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b'),
            Array('b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a'),
            Array('a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b'),
            Array('b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a'),
            Array('a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b')
          ),
          Array(
            "ababababaa"
            // "ababababab",
            // "ababababac",
            // "ababababad",
            // "ababababae",
            // "ababababaf",
            // "ababababag",
            // "ababababah",
            // "ababababai",
            // "ababababaj",
            // "ababababak",
            // "ababababal",
            // "ababababam",
            // "ababababan",
            // "ababababao",
            // "ababababap",
            // "ababababaq",
            // "ababababar",
            // "ababababas",
            // "ababababat",
            // "ababababau",
            // "ababababav",
            // "ababababaw",
            // "ababababax",
            // "ababababay",
            // "ababababaz",
            // "ababababba",
            // "ababababbb",
            // "ababababbc",
            // "ababababbd",
            // "ababababbe",
            // "ababababbf",
            // "ababababbg",
            // "ababababbh",
            // "ababababbi",
            // "ababababbj",
            // "ababababbk",
            // "ababababbl",
            // "ababababbm",
            // "ababababbn",
            // "ababababbo",
            // "ababababbp",
            // "ababababbq",
            // "ababababbr",
            // "ababababbs",
            // "ababababbt",
            // "ababababbu",
            // "ababababbv",
            // "ababababbw",
            // "ababababbx",
            // "ababababby",
            // "ababababbz",
            // "ababababca",
            // "ababababcb",
            // "ababababcc",
            // "ababababcd",
            // "ababababce",
            // "ababababcf",
            // "ababababcg",
            // "ababababch",
            // "ababababci",
            // "ababababcj",
            // "ababababck",
            // "ababababcl",
            // "ababababcm",
            // "ababababcn",
            // "ababababco",
            // "ababababcp",
            // "ababababcq",
            // "ababababcr",
            // "ababababcs",
            // "ababababct",
            // "ababababcu",
            // "ababababcv",
            // "ababababcw",
            // "ababababcx",
            // "ababababcy",
            // "ababababcz",
            // "ababababda",
            // "ababababdb",
            // "ababababdc",
            // "ababababdd",
            // "ababababde",
            // "ababababdf",
            // "ababababdg",
            // "ababababdh",
            // "ababababdi",
            // "ababababdj",
            // "ababababdk",
            // "ababababdl",
            // "ababababdm",
            // "ababababdn",
            // "ababababdo",
            // "ababababdp",
            // "ababababdq",
            // "ababababdr",
            // "ababababds",
            // "ababababdt",
            // "ababababdu",
            // "ababababdv",
            // "ababababdw",
            // "ababababdx",
            // "ababababdy",
            // "ababababdz",
            // "ababababea",
            // "ababababeb",
            // "ababababec",
            // "ababababed",
            // "ababababee",
            // "ababababef",
            // "ababababeg",
            // "ababababeh",
            // "ababababei",
            // "ababababej",
            // "ababababek",
            // "ababababel",
            // "ababababem",
            // "ababababen",
            // "ababababeo",
            // "ababababep",
            // "ababababeq",
            // "ababababer",
            // "ababababes",
            // "ababababet",
            // "ababababeu",
            // "ababababev",
            // "ababababew",
            // "ababababex",
            // "ababababey",
            // "ababababez",
            // "ababababfa",
            // "ababababfb",
            // "ababababfc",
            // "ababababfd",
            // "ababababfe",
            // "ababababff",
            // "ababababfg",
            // "ababababfh",
            // "ababababfi",
            // "ababababfj",
            // "ababababfk",
            // "ababababfl",
            // "ababababfm",
            // "ababababfn",
            // "ababababfo",
            // "ababababfp",
            // "ababababfq",
            // "ababababfr",
            // "ababababfs",
            // "ababababft",
            // "ababababfu",
            // "ababababfv",
            // "ababababfw",
            // "ababababfx",
            // "ababababfy",
            // "ababababfz",
            // "ababababga",
            // "ababababgb",
            // "ababababgc",
            // "ababababgd",
            // "ababababge",
            // "ababababgf",
            // "ababababgg",
            // "ababababgh",
            // "ababababgi",
            // "ababababgj",
            // "ababababgk",
            // "ababababgl",
            // "ababababgm",
            // "ababababgn",
            // "ababababgo",
            // "ababababgp",
            // "ababababgq",
            // "ababababgr",
            // "ababababgs",
            // "ababababgt",
            // "ababababgu",
            // "ababababgv",
            // "ababababgw",
            // "ababababgx",
            // "ababababgy",
            // "ababababgz",
            // "ababababha",
            // "ababababhb",
            // "ababababhc",
            // "ababababhd",
            // "ababababhe",
            // "ababababhf",
            // "ababababhg",
            // "ababababhh",
            // "ababababhi",
            // "ababababhj",
            // "ababababhk",
            // "ababababhl",
            // "ababababhm",
            // "ababababhn",
            // "ababababho",
            // "ababababhp",
            // "ababababhq",
            // "ababababhr",
            // "ababababhs",
            // "ababababht",
            // "ababababhu",
            // "ababababhv",
            // "ababababhw",
            // "ababababhx",
            // "ababababhy",
            // "ababababhz",
            // "ababababia",
            // "ababababib",
            // "ababababic",
            // "ababababid",
            // "ababababie",
            // "ababababif",
            // "ababababig",
            // "ababababih",
            // "ababababii",
            // "ababababij",
            // "ababababik",
            // "ababababil",
            // "ababababim",
            // "ababababin",
            // "ababababio",
            // "ababababip",
            // "ababababiq",
            // "ababababir",
            // "ababababis",
            // "ababababit",
            // "ababababiu",
            // "ababababiv",
            // "ababababiw",
            // "ababababix",
            // "ababababiy",
            // "ababababiz",
            // "ababababja",
            // "ababababjb",
            // "ababababjc",
            // "ababababjd",
            // "ababababje",
            // "ababababjf",
            // "ababababjg",
            // "ababababjh",
            // "ababababji",
            // "ababababjj",
            // "ababababjk",
            // "ababababjl",
            // "ababababjm",
            // "ababababjn",
            // "ababababjo",
            // "ababababjp",
            // "ababababjq",
            // "ababababjr",
            // "ababababjs",
            // "ababababjt",
            // "ababababju",
            // "ababababjv",
            // "ababababjw",
            // "ababababjx",
            // "ababababjy",
            // "ababababjz",
            // "ababababka",
            // "ababababkb",
            // "ababababkc",
            // "ababababkd",
            // "ababababke",
            // "ababababkf",
            // "ababababkg",
            // "ababababkh",
            // "ababababki",
            // "ababababkj",
            // "ababababkk",
            // "ababababkl",
            // "ababababkm",
            // "ababababkn",
            // "ababababko",
            // "ababababkp",
            // "ababababkq",
            // "ababababkr",
            // "ababababks",
            // "ababababkt",
            // "ababababku",
            // "ababababkv",
            // "ababababkw",
            // "ababababkx",
            // "ababababky",
            // "ababababkz",
            // "ababababla",
            // "abababablb",
            // "abababablc",
            // "ababababld",
            // "abababable",
            // "abababablf",
            // "abababablg",
            // "abababablh",
            // "ababababli",
            // "abababablj",
            // "abababablk",
            // "ababababll",
            // "abababablm",
            // "ababababln",
            // "abababablo",
            // "abababablp",
            // "abababablq",
            // "abababablr",
            // "ababababls",
            // "abababablt",
            // "abababablu",
            // "abababablv",
            // "abababablw",
            // "abababablx",
            // "abababably",
            // "abababablz",
            // "ababababma",
            // "ababababmb",
            // "ababababmc",
            // "ababababmd",
            // "ababababme",
            // "ababababmf",
            // "ababababmg",
            // "ababababmh",
            // "ababababmi",
            // "ababababmj",
            // "ababababmk",
            // "ababababml",
            // "ababababmm",
            // "ababababmn",
            // "ababababmo",
            // "ababababmp",
            // "ababababmq",
            // "ababababmr",
            // "ababababms",
            // "ababababmt",
            // "ababababmu",
            // "ababababmv",
            // "ababababmw",
            // "ababababmx",
            // "ababababmy",
            // "ababababmz",
            // "ababababna",
            // "ababababnb",
            // "ababababnc",
            // "ababababnd",
            // "ababababne",
            // "ababababnf",
            // "ababababng",
            // "ababababnh",
            // "ababababni",
            // "ababababnj",
            // "ababababnk",
            // "ababababnl",
            // "ababababnm",
            // "ababababnn",
            // "ababababno",
            // "ababababnp",
            // "ababababnq",
            // "ababababnr",
            // "ababababns",
            // "ababababnt",
            // "ababababnu",
            // "ababababnv",
            // "ababababnw",
            // "ababababnx",
            // "ababababny",
            // "ababababnz",
            // "ababababoa",
            // "ababababob",
            // "ababababoc",
            // "ababababod",
            // "ababababoe",
            // "ababababof",
            // "ababababog",
            // "ababababoh",
            // "ababababoi",
            // "ababababoj",
            // "ababababok",
            // "ababababol",
            // "ababababom",
            // "ababababon",
            // "ababababoo",
            // "ababababop",
            // "ababababoq",
            // "ababababor",
            // "ababababos",
            // "ababababot",
            // "ababababou",
            // "ababababov",
            // "ababababow",
            // "ababababox",
            // "ababababoy",
            // "ababababoz",
            // "ababababpa",
            // "ababababpb",
            // "ababababpc",
            // "ababababpd",
            // "ababababpe",
            // "ababababpf",
            // "ababababpg",
            // "ababababph",
            // "ababababpi",
            // "ababababpj",
            // "ababababpk",
            // "ababababpl",
            // "ababababpm",
            // "ababababpn",
            // "ababababpo",
            // "ababababpp",
            // "ababababpq",
            // "ababababpr",
            // "ababababps",
            // "ababababpt",
            // "ababababpu",
            // "ababababpv",
            // "ababababpw",
            // "ababababpx",
            // "ababababpy",
            // "ababababpz",
            // "ababababqa",
            // "ababababqb",
            // "ababababqc",
            // "ababababqd",
            // "ababababqe",
            // "ababababqf",
            // "ababababqg",
            // "ababababqh",
            // "ababababqi",
            // "ababababqj",
            // "ababababqk",
            // "ababababql",
            // "ababababqm",
            // "ababababqn",
            // "ababababqo",
            // "ababababqp",
            // "ababababqq",
            // "ababababqr",
            // "ababababqs",
            // "ababababqt",
            // "ababababqu",
            // "ababababqv",
            // "ababababqw",
            // "ababababqx",
            // "ababababqy",
            // "ababababqz",
            // "ababababra",
            // "ababababrb",
            // "ababababrc",
            // "ababababrd",
            // "ababababre",
            // "ababababrf",
            // "ababababrg",
            // "ababababrh",
            // "ababababri",
            // "ababababrj",
            // "ababababrk",
            // "ababababrl",
            // "ababababrm",
            // "ababababrn",
            // "ababababro",
            // "ababababrp",
            // "ababababrq",
            // "ababababrr",
            // "ababababrs",
            // "ababababrt",
            // "ababababru",
            // "ababababrv",
            // "ababababrw",
            // "ababababrx",
            // "ababababry",
            // "ababababrz",
            // "ababababsa",
            // "ababababsb",
            // "ababababsc",
            // "ababababsd",
            // "ababababse",
            // "ababababsf",
            // "ababababsg",
            // "ababababsh",
            // "ababababsi",
            // "ababababsj",
            // "ababababsk",
            // "ababababsl",
            // "ababababsm",
            // "ababababsn",
            // "ababababso",
            // "ababababsp",
            // "ababababsq",
            // "ababababsr",
            // "ababababss",
            // "ababababst",
            // "ababababsu",
            // "ababababsv",
            // "ababababsw",
            // "ababababsx",
            // "ababababsy",
            // "ababababsz",
            // "ababababta",
            // "ababababtb",
            // "ababababtc",
            // "ababababtd",
            // "ababababte",
            // "ababababtf",
            // "ababababtg",
            // "ababababth",
            // "ababababti",
            // "ababababtj",
            // "ababababtk",
            // "ababababtl",
            // "ababababtm",
            // "ababababtn",
            // "ababababto",
            // "ababababtp",
            // "ababababtq",
            // "ababababtr",
            // "ababababts",
            // "ababababtt",
            // "ababababtu",
            // "ababababtv",
            // "ababababtw",
            // "ababababtx",
            // "ababababty",
            // "ababababtz",
            // "ababababua",
            // "ababababub",
            // "ababababuc",
            // "ababababud",
            // "ababababue",
            // "ababababuf",
            // "ababababug",
            // "ababababuh",
            // "ababababui",
            // "ababababuj",
            // "ababababuk",
            // "ababababul",
            // "ababababum",
            // "ababababun",
            // "ababababuo",
            // "ababababup",
            // "ababababuq",
            // "ababababur",
            // "ababababus",
            // "ababababut",
            // "ababababuu",
            // "ababababuv",
            // "ababababuw",
            // "ababababux",
            // "ababababuy",
            // "ababababuz",
            // "ababababva",
            // "ababababvb",
            // "ababababvc",
            // "ababababvd",
            // "ababababve",
            // "ababababvf",
            // "ababababvg",
            // "ababababvh",
            // "ababababvi",
            // "ababababvj",
            // "ababababvk",
            // "ababababvl",
            // "ababababvm",
            // "ababababvn",
            // "ababababvo",
            // "ababababvp",
            // "ababababvq",
            // "ababababvr",
            // "ababababvs",
            // "ababababvt",
            // "ababababvu",
            // "ababababvv",
            // "ababababvw",
            // "ababababvx",
            // "ababababvy",
            // "ababababvz",
            // "ababababwa",
            // "ababababwb",
            // "ababababwc",
            // "ababababwd",
            // "ababababwe",
            // "ababababwf",
            // "ababababwg",
            // "ababababwh",
            // "ababababwi",
            // "ababababwj",
            // "ababababwk",
            // "ababababwl",
            // "ababababwm",
            // "ababababwn",
            // "ababababwo",
            // "ababababwp",
            // "ababababwq",
            // "ababababwr",
            // "ababababws",
            // "ababababwt",
            // "ababababwu",
            // "ababababwv",
            // "ababababww",
            // "ababababwx",
            // "ababababwy",
            // "ababababwz",
            // "ababababxa",
            // "ababababxb",
            // "ababababxc",
            // "ababababxd",
            // "ababababxe",
            // "ababababxf",
            // "ababababxg",
            // "ababababxh",
            // "ababababxi",
            // "ababababxj",
            // "ababababxk",
            // "ababababxl",
            // "ababababxm",
            // "ababababxn",
            // "ababababxo",
            // "ababababxp",
            // "ababababxq",
            // "ababababxr",
            // "ababababxs",
            // "ababababxt",
            // "ababababxu",
            // "ababababxv",
            // "ababababxw",
            // "ababababxx",
            // "ababababxy",
            // "ababababxz",
            // "ababababya",
            // "ababababyb",
            // "ababababyc",
            // "ababababyd",
            // "ababababye",
            // "ababababyf",
            // "ababababyg",
            // "ababababyh",
            // "ababababyi",
            // "ababababyj",
            // "ababababyk",
            // "ababababyl",
            // "ababababym",
            // "ababababyn",
            // "ababababyo",
            // "ababababyp",
            // "ababababyq",
            // "ababababyr",
            // "ababababys",
            // "ababababyt",
            // "ababababyu",
            // "ababababyv",
            // "ababababyw",
            // "ababababyx",
            // "ababababyy",
            // "ababababyz",
            // "ababababza",
            // "ababababzb",
            // "ababababzc",
            // "ababababzd",
            // "ababababze",
            // "ababababzf",
            // "ababababzg",
            // "ababababzh",
            // "ababababzi",
            // "ababababzj",
            // "ababababzk",
            // "ababababzl",
            // "ababababzm",
            // "ababababzn",
            // "ababababzo",
            // "ababababzp",
            // "ababababzq",
            // "ababababzr",
            // "ababababzs",
            // "ababababzt",
            // "ababababzu",
            // "ababababzv",
            // "ababababzw",
            // "ababababzx",
            // "ababababzy",
            // "ababababzz"
          )
        )
        .sameElements(Array("ababababab"))
    )
  }
}
