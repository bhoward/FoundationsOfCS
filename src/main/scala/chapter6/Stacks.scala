package chapter6

// Based on Figures 6.17 and 6.18 of Aho & Ullman
object Stacks {
  trait Stack {
    def clear(): Unit
    def isEmpty: Boolean
    def isFull: Boolean
    def peek: Option[Int]
    def pop(): Boolean
    def push(x: Int): Boolean
  }

  class ArrayStack extends Stack {
    private val MAX = 100
    private val A = new Array[Int](MAX)
    private var top = -1

    def clear(): Unit = {
      top = -1
    }

    def isEmpty: Boolean = top < 0

    def isFull: Boolean = top == MAX - 1

    def peek: Option[Int] = {
      if (isEmpty) {
        None
      } else {
        Option(A(top))
      }
    }

    def pop(): Boolean = {
      if (isEmpty) {
        false
      } else {
        top -= 1
        true
      }
    }

    def push(x: Int): Boolean = {
      if (isFull) {
        false
      } else {
        top += 1
        A(top) = x
        true
      }
    }
  }

  class ListStack extends Stack {
    private var L: List[Int] = Nil

    def clear(): Unit = {
      L = Nil
    }

    def isEmpty: Boolean = L == Nil

    def isFull: Boolean = false

    def peek: Option[Int] = L match {
      case Nil       => None
      case head :: _ => Option(head)
    }

    def pop(): Boolean = L match {
      case Nil => false
      case _ :: tail => {
        L = tail
        true
      }
    }

    def push(x: Int): Boolean = {
      L = x :: L
      true
    }
  }

  class MutableListStack extends Stack {
    class LIST(val element: Int, var next: LIST)
    val NULL: LIST = null // scalastyle:ignore

    private var L: LIST = NULL

    def clear(): Unit = {
      L = NULL
    }

    def isEmpty: Boolean = L == NULL

    def isFull: Boolean = false

    def peek: Option[Int] = {
      if (isEmpty) {
        None
      } else {
        Option(L.element)
      }
    }

    def pop(): Boolean = {
      if (isEmpty) {
        false
      } else {
        L = L.next
        true
      }
    }

    def push(x: Int): Boolean = {
      L = new LIST(x, L)
      true
    }
  }

  class DoublyLinkedListStack extends Stack {
    import DoublyLinkedList._

    private val L = empty

    def clear(): Unit = DoublyLinkedList.clear(L)

    def isEmpty: Boolean = DoublyLinkedList.isEmpty(L)

    def isFull: Boolean = false

    def peek: Option[Int] = front(L)

    def pop(): Boolean = !isEmpty && { deleteFront(L); true }

    def push(x: Int) = { insertFront(x, L); true }
  }
}
