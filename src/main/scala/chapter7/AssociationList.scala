package chapter7

// Based on Figure 7.18 of Aho & Ullman
object AssociationList {
  type AList[Key, Value] = List[(Key, Value)]

  def insert[Key, Value](key: Key, value: Value, alist: AList[Key, Value]): AList[Key, Value] = alist match {
    case Nil => (key, value) :: Nil
    case (k, v) :: tail =>
      if (k == key) { // update existing pair
        (key, value) :: tail
      } else {
        (k, v) :: insert(key, value, tail)
      }
  }

  def lookup[Key, Value](key: Key, alist: AList[Key, Value]): Option[Value] = alist match {
    case Nil => None
    case (k, v) :: tail =>
      if (k == key) {
        Option(v)
      } else {
        lookup(key, tail)
      }
  }

  def delete[Key, Value](key: Key, alist: AList[Key, Value]): AList[Key, Value] = alist match {
    case Nil => Nil
    case (k, v) :: tail =>
      if (k == key) {
        tail
      } else {
        (k, v) :: delete(key, tail)
      }
  }
}
