package chapter7

import util.TestSpec

class ListSetTest extends TestSpec {
  import ListSet._
  
  "Sorted List Set Union" should "contain all elements of both sets" in {
    forAll { (xs: List[Int], ys: List[Int]) =>
      var X: SET = Nil
      for (x <- xs) X = union(X, List(x))
      var Y: SET = Nil
      for (y <- ys) Y = union(Y, List(y))
      
      val XunionY = union(X, Y)
      xs.forall(x => XunionY contains x) should be(true)
      ys.forall(y => XunionY contains y) should be(true)
      forAll { n: Int =>
        XunionY contains n should be((xs contains n) || (ys contains n))
      }
    }
  }
  
  "Sorted List Set Intersection" should "contain only common elements of both sets" in {
    forAll { (xs: List[Int], ys: List[Int]) =>
      var X: SET = Nil
      for (x <- xs) X = union(X, List(x))
      var Y: SET = Nil
      for (y <- ys) Y = union(Y, List(y))
      
      val XintersectY = intersection(X, Y)
      xs.forall(x => !(ys contains x) || (XintersectY contains x)) should be(true)
      ys.forall(y => !(xs contains y) || (XintersectY contains y)) should be(true)
      forAll { n: Int =>
        XintersectY contains n should be((xs contains n) && (ys contains n))
      }
    }
  }
}
