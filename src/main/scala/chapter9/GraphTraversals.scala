package chapter9

// Based on Figures 9.27, 9.31, 9.33, and 9.44-47 of Aho & Ullman
object GraphTraversals {
  // Adjacency list representation of directed graph
  type GRAPH[Node] = Map[Node, Set[Node]]

  case class TREE[Node](root: Node, children: FOREST[Node])
  type FOREST[Node] = List[TREE[Node]]

  sealed trait DFSResult
  case object Cyclic extends DFSResult
  case class TopologicalOrder[Node](nodes: List[Node]) extends DFSResult

  // recursive version:
  def dfs[Node](graph: GRAPH[Node]): (FOREST[Node], DFSResult) = {
    var visited = Set[Node]()
    var finished = List[Node]()
    var cycle = false

    def aux(nodes: Set[Node]): FOREST[Node] = nodes.foldLeft(Nil: FOREST[Node]) {
      case (f, n) =>
        if (visited contains n) {
          if (!(finished contains n)) cycle = true
          f
        } else {
          visited += n
          val children = aux(graph(n)).reverse
          finished ::= n
          TREE(n, children) :: f
        }
    }

    val forest = aux(graph.keySet).reverse
    val result = if (cycle) Cyclic else TopologicalOrder(finished)
    (forest, result)
  }

  // iterative version, with a stack
  def dfsStack[Node](graph: GRAPH[Node]): (FOREST[Node], DFSResult) = {
    import scala.collection.mutable.Stack

    sealed trait Action
    case class Visit(node: Node) extends Action
    case class Finish(node: Node) extends Action

    var visited = Set[Node]()
    var finished = List[Node]()
    var cycle = false

    val stack = Stack[Action]()
    val trees = Stack[FOREST[Node]](Nil)
    for (n <- graph.keySet) stack.push(Visit(n))
    while (stack.nonEmpty) {
      stack.pop() match {
        case Visit(n) =>
          if (visited contains n) {
            if (!(finished contains n)) cycle = true
          } else {
            visited += n
            stack.push(Finish(n))
            trees.push(Nil)
            for (n1 <- graph(n)) stack.push(Visit(n1))
          }
        case Finish(n) =>
          finished ::= n
          val children = trees.pop().reverse
          val siblings = trees.pop()
          trees.push(TREE(n, children) :: siblings)
      }
    }

    val forest = trees.pop().reverse
    val result = if (cycle) Cyclic else TopologicalOrder(finished)
    (forest, result)
  }

  def bfsQueue[Node](start: Node, graph: GRAPH[Node]): List[Node] = {
    import scala.collection.mutable.Queue

    var visited = List[Node]()

    val queue = Queue[Node]()
    queue.enqueue(start)
    while (queue.nonEmpty) {
      queue.dequeue() match {
        case n =>
          if (visited contains n) {
          } else {
            visited ::= n
            for (n1 <- graph(n)) queue.enqueue(n1)
          }
      }
    }

    visited.reverse
  }
}
