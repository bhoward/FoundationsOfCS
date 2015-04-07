package chapter7

// Based on Figures 7.11, 7.14, and 7.22 of Aho & Ullman
object HashTable {
  val NUMBER_OF_BUCKETS = 5

  def hash(s: String): Int = {
    var sum = 0
    for (c <- s) sum += c
    sum % NUMBER_OF_BUCKETS
  }

  type TABLE[Value] = Array[List[(String, Value)]]

  def insert[Value](key: String, value: Value, table: TABLE[Value]): Unit = {
    def aux(bucket: List[(String, Value)]): List[(String, Value)] = bucket match {
      case Nil => List((key, value))
      case (k, v) :: tail =>
        if (k == key) {
          (key, value) :: tail
        } else {
          (k, v) :: aux(tail)
        }
    }

    val h = hash(key)
    table(h) = aux(table(h))
  }

  def delete[Value](key: String, table: TABLE[Value]): Unit = {
    def aux(bucket: List[(String, Value)]): List[(String, Value)] = bucket match {
      case Nil => Nil
      case (k, v) :: tail =>
        if (k == key) {
          tail
        } else {
          (k, v) :: aux(tail)
        }
    }

    val h = hash(key)
    table(h) = aux(table(h))
  }

  def lookup[Value](key: String, table: TABLE[Value]): Option[Value] = {
    def aux(bucket: List[(String, Value)]): Option[Value] = bucket match {
      case Nil => None
      case (k, v) :: tail =>
        if (k == key) {
          Option(v)
        } else {
          aux(tail)
        }
    }

    val h = hash(key)
    aux(table(h))
  }
}
