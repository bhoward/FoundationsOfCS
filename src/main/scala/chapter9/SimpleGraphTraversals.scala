package chapter9

object SimpleGraphTraversals {
  type Node = String
  
    // Adjacency list representation of unweighted directed graph
  type GRAPH = Map[Node, Set[Node]]

  case class TREE(root: Node, children: FOREST)
  type FOREST = List[TREE]

  sealed trait DFSResult
  case object Cyclic extends DFSResult
  case class TopologicalOrder(nodes: List[Node]) extends DFSResult

  // recursive version:
  def dfs(graph: GRAPH): (FOREST, DFSResult) = {
    var visited = Set[Node]()
    var finished = List[Node]()
    var cycle = false

    def aux(nodes: Set[Node]): FOREST = {
      var forest: FOREST = Nil
      for (n <- nodes) {
        if (visited contains n) {
          if (!(finished contains n)) cycle = true
        } else {
          visited += n
          val children = aux(graph(n))
          finished = n :: finished
          forest = forest :+ TREE(n, children)
        }
      }
      
      forest
    }

    val forest = aux(graph.keySet)
    val result = if (cycle) Cyclic else TopologicalOrder(finished)
    (forest, result)
  }

  // iterative version, with a stack
  def dfsStack(graph: GRAPH): (FOREST, DFSResult) = {
    import scala.collection.mutable.Stack

    sealed trait Action
    case class Visit(node: Node) extends Action
    case class Finish(node: Node) extends Action

    var visited = Set[Node]()
    var finished = List[Node]()
    var cycle = false

    val stack = Stack[Action]()
    val trees = Stack[FOREST](Nil)
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
          finished = n :: finished
          val children = trees.pop()
          val siblings = trees.pop()
          trees.push(siblings :+ TREE(n, children))
      }
    }

    val forest = trees.pop()
    val result = if (cycle) Cyclic else TopologicalOrder(finished)
    (forest, result)
  }

  def bfsQueue(start: Node, graph: GRAPH): Map[Node, List[Node]] = {
    import scala.collection.mutable.Queue

    var visited = Map[Node, List[Node]]()

    val queue = Queue[(Node, List[Node])]()
    queue.enqueue(start -> Nil)
    
    while (queue.nonEmpty) {
      queue.dequeue() match {
        case (n, path) =>
          if (visited contains n) {
            // already visited; ignore
          } else {
            visited += n -> path
            val path2 = n :: path
            for (n1 <- graph(n)) queue.enqueue(n1 -> path2)
          }
      }
    }

    visited
  }
}