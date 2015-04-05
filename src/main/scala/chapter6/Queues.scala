package chapter6

// Based on Figure 6.29 of Aho & Ullman
object Queues {
  trait Queue {
    def clear(): Unit
    def isEmpty: Boolean
    def isFull: Boolean
    def peek: Option[Int]
    def dequeue(): Boolean
    def enqueue(x: Int): Boolean
  }

  class ArrayQueue extends Queue {
    private val MAX = 100
    private val A = new Array[Int](MAX)

    // Invariant: either front == back, or
    // first is A(front), last is A(if (back == 0) MAX else back - 1);
    // front and back are always in range 0 .. MAX-1
    private var front = 0
    private var back = 0

    def clear(): Unit = {
      front = 0
      back = 0
    }

    def isEmpty: Boolean = front == back

    def isFull: Boolean = front == (back + 1) % MAX

    def peek: Option[Int] = {
      if (isEmpty) {
        None
      } else {
        Option(A(front))
      }
    }

    def dequeue(): Boolean = {
      if (isEmpty) {
        false
      } else {
        front = (front + 1) % MAX
        true
      }
    }

    def enqueue(x: Int): Boolean = {
      if (isFull) {
        false
      } else {
        A(back) = x
        back = (back + 1) % MAX
        true
      }
    }
  }

  class ListQueue extends Queue {
    // Invariant: queue is front ::: back.reverse
    private var front: List[Int] = Nil
    private var back: List[Int] = Nil

    def clear(): Unit = {
      front = Nil
      back = Nil
    }

    def isEmpty: Boolean = front == Nil && back == Nil

    def isFull: Boolean = false

    def peek: Option[Int] = front match {
      case Nil => {
        if (back == Nil) {
          None
        } else {
          front = back.reverse
          back = Nil
          Option(front.head)
        }
      }
      case head :: _ => Option(head)
    }

    def dequeue(): Boolean = front match {
      case Nil => {
        if (back == Nil) {
          false
        } else {
          front = back.reverse.tail
          back = Nil
          true
        }
      }
      case _ :: tail => {
        front = tail
        true
      }
    }

    def enqueue(x: Int): Boolean = {
      back = x :: back
      true
    }
  }

  class MutableListQueue extends Queue {
    class LIST(val element: Int, var next: LIST)
    val NULL: LIST = null // scalastyle:ignore

    // Invariant: either both L and back are NULL, or
    // L refers to first node and back refers to last
    private var L: LIST = NULL
    private var back: LIST = NULL

    def clear(): Unit = {
      L = NULL
      back = NULL
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

    def dequeue(): Boolean = {
      if (isEmpty) {
        false
      } else {
        L = L.next
        if (L == NULL) back = NULL
        true
      }
    }

    def enqueue(x: Int): Boolean = {
      if (isEmpty) {
        L = new LIST(x, NULL)
        back = L
      } else {
        back.next = new LIST(x, NULL)
        back = back.next
      }
      true
    }
  }

  class DoublyLinkedListQueue extends Queue {
    import DoublyLinkedList._

    private val L = empty

    def clear(): Unit = DoublyLinkedList.clear(L)

    def isEmpty: Boolean = DoublyLinkedList.isEmpty(L)

    def isFull: Boolean = false

    def peek: Option[Int] = front(L)

    def dequeue(): Boolean = !isEmpty && { deleteFront(L); true }

    def enqueue(x: Int): Boolean = { insertBack(x, L); true }
  }
}
