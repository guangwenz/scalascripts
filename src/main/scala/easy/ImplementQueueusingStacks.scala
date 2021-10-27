package easy

/** Implement Queue using Stacks
  * Implement a first in first out (FIFO) queue using only two stacks. The implemented queue should support all the functions of a normal queue (push, peek, pop, and empty).
  *
  * Implement the MyQueue class:
  *
  * void push(int x) Pushes element x to the back of the queue.
  * int pop() Removes the element from the front of the queue and returns it.
  * int peek() Returns the element at the front of the queue.
  * boolean empty() Returns true if the queue is empty, false otherwise.
  * Notes:
  *
  * You must use only standard operations of a stack, which means only push to top, peek/pop from top, size, and is empty operations are valid.
  * Depending on your language, the stack may not be supported natively. You may simulate a stack using a list or deque (double-ended queue) as long as you use only a stack's standard operations.
  *
  * Constraints:
  *
  * 1 <= x <= 9
  * At most 100 calls will be made to push, pop, peek, and empty.
  * All the calls to pop and peek are valid.
  *
  * https://leetcode.com/problems/implement-queue-using-stacks/
  */
trait ImplementQueueusingStacks {
  class MyQueue() {
    val Q1 = collection.mutable.Stack.empty[Int]
    val Q2 = collection.mutable.Stack.empty[Int]

    def push(x: Int) {
      Q1.push(x)
    }

    def pop(): Int = {
      while (Q1.nonEmpty) Q2.push(Q1.pop())
      val ret = Q2.pop()
      while (Q2.nonEmpty) Q1.push(Q2.pop())
      ret
    }

    def peek(): Int = {
      while (Q1.nonEmpty) {
        Q2.push(Q1.pop())
      }
      val ret = Q2.top
      while (Q2.nonEmpty) {
        Q1.push(Q2.pop())
      }
      ret
    }

    def empty(): Boolean = {
      Q1.isEmpty
    }

  }

  /** Your MyQueue object will be instantiated and called as such:
    * var obj = new MyQueue()
    * obj.push(x)
    * var param_2 = obj.pop()
    * var param_3 = obj.peek()
    * var param_4 = obj.empty()
    */
  def run() = {
    val queue = new MyQueue()
    queue.push(1)
    queue.push(2)
    println(queue.peek() == 1)
    println(queue.pop() == 1)
    println(queue.empty() == false)
  }
}
