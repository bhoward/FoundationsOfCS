package chapter7

// Based on Figures 7.11, 7.14, and 7.22 of Aho & Ullman
object HashTable {
  import AssociationList.AList
  
  val NUMBER_OF_BUCKETS = 5

  def hash(s: String): Int = {
    var sum = 0
    for (c <- s) sum += c
    sum % NUMBER_OF_BUCKETS
  }

  type TABLE[Value] = Array[AList[String, Value]]

  def insert[Value](key: String, value: Value, table: TABLE[Value]): Unit = {
    val h = hash(key)
    table(h) = AssociationList.insert(key, value, table(h))
  }

  def delete[Value](key: String, table: TABLE[Value]): Unit = {
    val h = hash(key)
    table(h) = AssociationList.delete(key, table(h))
  }

  def lookup[Value](key: String, table: TABLE[Value]): Option[Value] = {
    val h = hash(key)
    AssociationList.lookup(key, table(h))
  }
}
