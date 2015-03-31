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

  def insert(t: BST, x: Int): BST = t match {
    case Empty => Node(Empty, x, Empty)
    case Node(l, v, r) =>
      if (x < v) {
        Node(insert(l, x), v, r)
      } else {
        // Note that this will insert a duplicate if x == v
        // Need duplicates for treeSort application
        Node(l, v, insert(r, x))
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

  def insertAll(t: BST, xs: List[Int]): BST = xs match {
    case Nil          => t
    case head :: tail => insertAll(insert(t, head), tail)
  }

  def inorder(t: BST): List[Int] = t match {
    case Empty         => Nil
    case Node(l, v, r) => inorder(l) ::: v :: inorder(r)
  }

  def treeSort(xs: List[Int]): List[Int] = inorder(insertAll(Empty, xs))
}
