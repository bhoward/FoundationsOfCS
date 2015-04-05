package chapter6

// Based on Figures 6.3, 6.4, and 6.5 of Aho & Ullman
// This uses an immutable list that is essentially the same
// as the built-in List[Int] type
object ImmutableLinkedList {
  sealed trait LIST
  case class CONS(element: Int, next: LIST) extends LIST
  case object NULL extends LIST

  def lookup(x: Int, L: LIST): Boolean = L match {
    case NULL => false
    case CONS(element, next) =>
      if (x == element) {
        true
      } else {
        lookup(x, next)
      }
  }

  def delete(x: Int, L: LIST): LIST = L match {
    case NULL => NULL
    case CONS(element, next) =>
      if (x == element) {
        next
      } else {
        CONS(element, delete(x, next))
      }
  }

  def insert(x: Int, L: LIST): LIST = L match {
    case NULL => CONS(x, NULL)
    case CONS(element, next) =>
      if (x != element) {
        CONS(element, insert(x, next))
      } else {
        L
      }
  }
}
