package chapter5

// Modifies plain BST to do auto-balancing, AVL-style
object AVLBinarySearchTree {
  sealed trait BST {
    val height: Int
  }
  case object Empty extends BST {
    val height: Int = -1
  }
  case class Node(left: BST, value: Int, right: BST) extends BST {
    val height: Int = 1 + (left.height max right.height)
  }

  // Use this to construct Nodes if they might need balancing.
  // AVL balance condition: diff between child heights is at most 1.
  def balance(left: BST, value: Int, right: BST): BST = {
    val diff = left.height - right.height
    if (math.abs(diff) > 2) sys.error("Balance out of whack: " + left + ", " + right)
    if (diff == -2) {
      // right is high
      val Node(rl, rv, rr) = right.asInstanceOf[Node]
      if (rl.height <= rr.height) { // single rotate left
        Node(Node(left, value, rl), rv, rr)
      } else { // double rotate right-left
        val Node(rll, rlv, rlr) = rl.asInstanceOf[Node]
        Node(Node(left, value, rll), rlv, Node(rlr, rv, rr))
      }
    } else if (diff == 2) {
      // left is high
      val Node(ll, lv, lr) = left.asInstanceOf[Node]
      if (ll.height >= lr.height) { // single rotate right
        Node(ll, lv, Node(lr, value, right))
      } else { // double rotate left-right
        val Node(lrl, lrv, lrr) = lr.asInstanceOf[Node]
        Node(Node(ll, lv, lrl), lrv, Node(lrr, value, right))
      }
    } else {
      // balance OK
      Node(left, value, right)
    }
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
        balance(insert(l, x), v, r)
      } else {
        balance(l, v, insert(r, x))
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
    case Node(l, v, r)     => balance(deleteMin(l), v, r)
  }

  def delete(t: BST, x: Int): BST = t match {
    case Empty => Empty
    case Node(l, v, r) =>
      if (x == v) {
        findMin(r) match {
          case None    => l
          case Some(m) => balance(l, m, deleteMin(r))
        }
      } else if (x < v) {
        balance(delete(l, x), v, r)
      } else {
        balance(l, v, delete(r, x))
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
