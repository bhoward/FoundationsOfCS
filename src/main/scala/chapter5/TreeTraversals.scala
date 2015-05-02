package chapter5

object TreeTraversals {
  sealed trait Tree
  case object Empty extends Tree
  case class Node(left: Tree, value: Int, right: Tree) extends Tree

  // Recursive Depth-First Traversals:
  def preorder(t: Tree, visit: Int => Unit): Unit = t match {
    case Empty => {}
    case Node(l, v, r) => {
      visit(v)
      preorder(l, visit)
      preorder(r, visit)
    }
  }

  def inorder(t: Tree, visit: Int => Unit): Unit = t match {
    case Empty => {}
    case Node(l, v, r) => {
      inorder(l, visit)
      visit(v)
      inorder(r, visit)
    }
  }

  def postorder(t: Tree, visit: Int => Unit): Unit = t match {
    case Empty => {}
    case Node(l, v, r) => {
      postorder(l, visit)
      postorder(r, visit)
      visit(v)
    }
  }

  trait TreeVisitor {
    def pre(v: Int): Unit
    def in(v: Int): Unit
    def post(v: Int): Unit
    def empty(): Unit
  }

  def traverse(t: Tree, visitor: TreeVisitor): Unit = t match {
    case Empty => visitor.empty()
    case Node(l, v, r) => {
      visitor.pre(v)
      traverse(l, visitor)
      visitor.in(v)
      traverse(r, visitor)
      visitor.post(v)
    }
  }

  // Iterative Traversals:
  import scala.collection.mutable.Stack

  def itPreorder(t: Tree, visit: Int => Unit): Unit = {
    val s = Stack.empty[Tree]
    s.push(t)
    while (s.nonEmpty) {
      s.pop() match {
        case Empty => {}
        case Node(l, v, r) => {
          visit(v)
          s.push(r)
          s.push(l)
        }
      }
    }
  }

  sealed trait Event
  case class Pre(t: Tree) extends Event
  case class In(v: Int) extends Event
  case class Post(v: Int) extends Event

  def itTraverse(t: Tree, visitor: TreeVisitor): Unit = {
    val s = Stack.empty[Event]
    s.push(Pre(t))
    while (s.nonEmpty) {
      s.pop() match {
        case Pre(Empty) => visitor.empty()
        case Pre(Node(l, v, r)) => {
          visitor.pre(v)
          s.push(Post(v))
          s.push(Pre(r))
          s.push(In(v))
          s.push(Pre(l))
        }
        case In(v) => {
          visitor.in(v)
        }
        case Post(v) => {
          visitor.post(v)
        }
      }
    }
  }

  // Iterative Level-Order:
  import scala.collection.mutable.Queue

  def itLevelorder(t: Tree, visit: Int => Unit): Unit = {
    val q = Queue.empty[Tree]
    q.enqueue(t)
    while (q.nonEmpty) {
      q.dequeue() match {
        case Empty => {}
        case Node(l, v, r) => {
          visit(v)
          q.enqueue(l)
          q.enqueue(r)
        }
      }
    }
  }
}