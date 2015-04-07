package chapter7

// Based on Figure 7.24 of Aho & Ullman
object ListRelation {
  type REL[Dom, Ran] = List[(Dom, Ran)]

  def insert[Dom, Ran](dom: Dom, ran: Ran, rel: REL[Dom, Ran]): REL[Dom, Ran] = rel match {
    case Nil => (dom, ran) :: Nil
    case (d, r) :: tail =>
      if (d == dom && r == ran) {
        rel
      } else {
        (d, r) :: insert(dom, ran, tail)
      }
  }

  def lookup[Dom, Ran](dom: Dom, rel: REL[Dom, Ran]): List[Ran] = rel match {
    case Nil => Nil
    case (d, r) :: tail =>
      if (d == dom) {
        r :: lookup(dom, tail)
      } else {
        lookup(dom, tail)
      }
  }

  def delete[Dom, Ran](dom: Dom, ran: Ran, rel: REL[Dom, Ran]): REL[Dom, Ran] = rel match {
    case Nil => Nil
    case (d, r) :: tail =>
      if (d == dom && r == ran) {
        tail
      } else {
        (d, r) :: delete(dom, ran, tail)
      }
  }
}
