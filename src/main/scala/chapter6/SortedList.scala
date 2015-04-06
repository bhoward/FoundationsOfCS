package chapter6

// Based on Figure 6.6 of Aho & Ullman, using the built-in List type
object SortedList {
  type LIST = List[Int]

  def lookup(x: Int, L: List[Int]): Boolean = L match {
    case Nil => false
    case element :: next =>
      if (x < element) { // x can't occur past this point
        false
      } else if (x == element) { // found
        true
      } else { // x > element -- keep looking
        lookup(x, next)
      }
  }

  def delete(x: Int, L: List[Int]): List[Int] = L match {
    case Nil => Nil
    case element :: next =>
      if (x < element) { // x can't occur past this point
        L
      } else if (x == element) { // found -- delete this
        next
      } else { // x > element -- keep looking
        element :: delete(x, next)
      }
  }

  def insert(x: Int, L: List[Int]): List[Int] = L match {
    case Nil => x :: Nil
    case element :: next =>
      if (x < element) { // x belongs here -- insert
        x :: L
      } else if (x == element) { // duplicate -- ignore
        L
      } else { // x > element -- keep looking
        element :: insert(x, next)
      }
  }
}
