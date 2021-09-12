package example

/** odd even number from linked list
  * https://leetcode.com/explore/interview/card/top-interview-questions-medium/107/linked-list/784/
  */
trait OddEvenNumber {
  import OddEvenNumber._

  def createNode(in: List[Int]): ListNode = {
    val root = new ListNode(1, null)
    in.foldLeft(root) { case (s, i) =>
      s.next = new ListNode(i)
      s.next
    }
    root.next
  }

  /** for loop
    */
  object Solution2 {
    def solve(in: ListNode): ListNode = {
      if (in == null) null
      else {
        var l1 = in
        var l2 = in.next
        var isEven = false
        var node = in

        while (node != null) {
          isEven = !isEven
          if (isEven)
            l1 = l2
          l2 = node.next
          node = node.next
        }

        ???
      }
    }
  }

  /** recursive solution
    */
  object Solution1 {
    def solve(in: ListNode): ListNode = {
      @annotation.tailrec
      def loop(
          node: Option[ListNode],
          last: Option[ListNode],
          isEven: Boolean,
          lastEven: Option[ListNode],
          evenHead: Option[ListNode]
      ): Option[ListNode] = {
        node match {
          case None =>
            last.foreach(_.next = evenHead.getOrElse(null))
            lastEven.foreach(_.next = null)
            Some(in)
          case Some(v) =>
            if (isEven) {
              val newLastEven = lastEven.fold(v) { n =>
                n.next = v
                v
              }
              last.foreach(_.next = v.next)
              loop(
                Option(v.next),
                last,
                !isEven,
                Some(newLastEven),
                Option(evenHead.getOrElse(newLastEven))
              )
            } else {
              loop(
                Option(v.next),
                Some(v),
                !isEven,
                lastEven,
                evenHead.orElse(lastEven)
              )
            }
        }
      }
      loop(Option(in), None, false, None, None).getOrElse(null)
    }
  }

  def printList(in: ListNode): Unit = {
    var ret = in
    println("=====")
    while (ret != null) {
      println(ret.x)
      ret = ret.next
    }
    println("=====")
  }
  def run() = {
    var ret = Solution1.solve(createNode((1 to 8).toList))
    // var ret = Solution1.solve(createNode(List(1, 1)))
    printList(ret)
  }
}

object OddEvenNumber {
  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }
}
