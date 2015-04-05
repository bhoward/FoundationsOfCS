package chapter6

// Loosely based on Figure 6.9 of Aho & Ullman
object DoublyLinkedList {
  // This is a circular doubly-linked list, with a dummy head node
  class LIST(var previous: LIST, val element: Int, var next: LIST)

  def empty: LIST = {
    val result = new LIST(null, 0, null) // scalastyle:ignore
    result.previous = result
    result.next = result
    result
  }

  def isEmpty(L: LIST): Boolean = L.next == L
  
  def clear(L: LIST): Unit = {
    L.next = L
    L.previous = L
  }

  def lookup(x: Int, L: LIST): Boolean = {
    var current = L.next
    while (current != L && current.element != x) {
      current = current.next
    }
    return current != L
  }
  
  def delete(x: Int, L: LIST): Unit = {
    var current = L.next
    while (current != L && current.element != x) {
      current = current.next
    }
    if (current != L) { // found
      current.previous.next = current.next
      current.next.previous = current.previous
    }
  }
  
  def front(L: LIST): Option[Int] = {
    if (isEmpty(L)) {
      None
    } else {
      Option(L.next.element)
    }
  }
  
  def back(L: LIST): Option[Int] = {
    if (isEmpty(L)) {
      None
    } else {
      Option(L.previous.element)
    }
  }
  
  def deleteFront(L: LIST): Unit = {
    L.next = L.next.next
    L.next.previous = L
  }
  
  def deleteBack(L: LIST): Unit = {
    L.previous = L.previous.previous
    L.previous.next = L
  }
  
  def insertFront(x: Int, L: LIST): Unit = {
    val node = new LIST(L, x, L.next)
    node.previous.next = node
    node.next.previous = node
  }

  def insertBack(x: Int, L: LIST): Unit = {
    val node = new LIST(L.previous, x, L)
    node.previous.next = node
    node.next.previous = node
  }
}