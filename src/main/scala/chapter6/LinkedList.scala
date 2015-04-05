package chapter6

// Based on Figures 6.3, 6.4, and 6.5 of Aho & Ullman
// This is a very imperative, mutable version of lists
object LinkedList {
  class LIST(val element: Int, var next: LIST)
  val NULL: LIST = null // scalastyle:ignore

  def lookup(x: Int, L: LIST): Boolean = {
    if (L == NULL) {
      false
    } else if (x == L.element) {
      true
    } else {
      lookup(x, L.next)
    }
  }

  // Returns the modified LIST, since Java/Scala don't have
  // reference parameters or explicit pointers
  def delete(x: Int, L: LIST): LIST = {
    if (L == NULL) {
      NULL
    } else if (x == L.element) {
      L.next
    } else {
      L.next = delete(x, L.next)
      L
    }
  }

  // Returns the modified LIST, as above
  def insert(x: Int, L: LIST): LIST = {
    if (L == NULL) {
      new LIST(x, NULL)
    } else if (x != L.element) {
      L.next = insert(x, L.next)
      L
    } else {
      // don't insert a duplicate
      L
    }
  }
}