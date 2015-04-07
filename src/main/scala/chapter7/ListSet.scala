package chapter7

// Based on Figures 7.6 and 7.8 of Aho & Ullman
object ListSet {
  // A set will be represented by a list sorted in (strictly) increasing order
  type SET = List[Int]

  def union(set1: SET, set2: SET): SET = (set1, set2) match {
    case (Nil, _) => set2
    case (_, Nil) => set1
    case (h1 :: t1, h2 :: t2) =>
      if (h1 < h2) {
        h1 :: union(t1, set2)
      } else if (h1 > h2) {
        h2 :: union(set1, t2)
      } else { // h1 == h2
        h1 :: union(t1, t2)
      }
  }

  def intersection(set1: SET, set2: SET): SET = (set1, set2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (h1 :: t1, h2 :: t2) =>
      if (h1 < h2) {
        intersection(t1, set2)
      } else if (h1 > h2) {
        intersection(set1, t2)
      } else { // h1 == h2
        h1 :: intersection(t1, t2)
      }
  }
}
