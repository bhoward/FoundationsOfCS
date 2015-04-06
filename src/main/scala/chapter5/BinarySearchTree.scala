package chapter5

// Based on Figures 5.34, 5.35, and 5.40 of Aho & Ullman
object BinarySearchTree {
  sealed trait BST
  case object Empty extends BST
  case class Node(left: BST, value: Int, right: BST) extends BST

  def height(t: BST): Int = t match {
    case Empty         => 0
    case Node(l, _, r) => 1 + (height(l) max height(r))
  }

  def contains(t: BST, x: Int): Boolean = t match {
    case Empty => false
    case Node(l, v, r) =>
      if (x == v) {
        true
      } else if (x < v) {
        contains(l, x)
      } else {
        contains(r, x)
      }
  }

  def findMin(t: BST): Option[Int] = t match {
    case Empty             => None
    case Node(Empty, v, _) => Some(v)
    case Node(l, _, _)     => findMin(l)
  }

  def deleteMin(t: BST): BST = t match {
    case Empty             => Empty
    case Node(Empty, _, r) => r
    case Node(l, v, r)     => Node(deleteMin(l), v, r)
  }

  def delete(t: BST, x: Int): BST = t match {
    case Empty => Empty
    case Node(l, v, r) =>
      if (x == v) {
        findMin(r) match {
          case None    => l
          case Some(m) => Node(l, m, deleteMin(r))
        }
      } else if (x < v) {
        Node(delete(l, x), v, r)
      } else {
        Node(l, v, delete(r, x))
      }
  }

  // naive version -- overflows stack with large unbalanced tree
  def insert0(t: BST, x: Int): BST = t match {
    case Empty => Node(Empty, x, Empty)
    case Node(l, v, r) =>
      if (x < v) {
        Node(insert0(l, x), v, r)
      } else {
        // Note that this will insert a duplicate if x == v
        // Need duplicates for treeSort application
        Node(l, v, insert0(r, x))
      }
  }

  // trampolined CPS version to avoid stack overflow
  def insert1(t: BST, x: Int): BST = {
    import scala.util.control.TailCalls.{TailRec, tailcall, done}

    def aux(t: BST, k: BST => TailRec[BST]): TailRec[BST] = t match {
      case Empty => tailcall(k(Node(Empty, x, Empty)))
      case Node(l, v, r) =>
        if (x < v) {
          aux(l, a => tailcall(k(Node(a, v, r))))
        } else {
          aux(r, a => tailcall(k(Node(l, v, a))))
        }
    }

    aux(t, a => done(a)).result
  }

  // naive version -- overflows stack with large unbalanced tree
  def inorder0(t: BST): List[Int] = t match {
    case Empty         => Nil
    case Node(l, v, r) => inorder0(l) ::: v :: inorder0(r)
  }

  // tail-recursive version to handle severely unbalanced trees
  def inorder1(t: BST): List[Int] = {
    def aux(ts: List[BST], result: List[Int]): List[Int] = ts match {
      case Nil                       => result
      case Empty :: tail             => aux(tail, result)
      case Node(l, v, Empty) :: tail => aux(l :: tail, v :: result)
      case Node(l, v, r) :: tail     => aux(r :: Node(Empty, v, Empty) :: l :: tail, result)
    }

    aux(List(t), Nil)
  }

  // choose the desired versions of insert and inorder
  def insert(t: BST, x: Int): BST = insert1(t, x)
  def inorder(t: BST): List[Int] = inorder1(t)

  def insertAll(t: BST, xs: List[Int]): BST = xs match {
    case Nil          => t
    case head :: tail => insertAll(insert(t, head), tail)
  }

  def treeSort(xs: List[Int]): List[Int] = inorder(insertAll(Empty, xs))

  def treeSortNaive(xs: List[Int]): List[Int] = inorder0(insertAll(Empty, xs))
  def treeSortTail(xs: List[Int]): List[Int] = inorder1(insertAll(Empty, xs))

  // The following versions were attempts at running faster without overflow on unbalanced trees,
  // preserved here to record what I tried -- they might still work with some tweaking
  //
  //  // CPS version
  //  def inorder2(t: BST): List[Int] = {
  //    def aux(t: BST, k: List[Int] => List[Int]): List[Int] = t match {
  //      case Empty => k(Nil)
  //      case Node(l, v, r) => aux(l, a => aux(r, b => k(a ::: v :: b)))
  //    }
  //
  //    aux(t, x => x)
  //  }
  //
  //  // Accumulator version
  //  def inorder3(t: BST): List[Int] = {
  //    def aux(t: BST, acc: List[Int]): List[Int] = t match {
  //      case Empty => acc
  //      case Node(l, v, r) => aux(l, v :: aux(r, acc))
  //    }
  //
  //    aux(t, Nil)
  //  }
  //
  //  // CPS-Accumulator version
  //  def inorder4(t: BST): List[Int] = {
  //    def aux(t: BST, acc: List[Int], k: List[Int] => List[Int]): List[Int] = t match {
  //      case Empty => k(acc)
  //      case Node(l, v, r) => aux(r, acc, a => aux(l, v :: a, k))
  //    }
  //
  //    aux(t, Nil, x => x)
  //  }
  //
  //  def treeSortCPS(xs: List[Int]): List[Int] = inorder2(insertAll(Empty, xs))
  //  def treeSortAcc(xs: List[Int]): List[Int] = inorder3(insertAll(Empty, xs))
  //  def treeSortCPSAcc(xs: List[Int]): List[Int] = inorder4(insertAll(Empty, xs))
}
