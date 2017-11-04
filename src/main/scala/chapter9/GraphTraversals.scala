package chapter9

// Based on Figures 9.27, 9.31, 9.33, and 9.44-47 of Aho & Ullman
object GraphTraversals {
  // Adjacency list representation of unweighted directed graph
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
          val children = aux(graph(n))
          finished ::= n
          f :+ TREE(n, children)
        }
    }

    val forest = aux(graph.keySet)
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
          val children = trees.pop()
          val siblings = trees.pop()
          trees.push(siblings :+ TREE(n, children))
      }
    }

    val forest = trees.pop()
    val result = if (cycle) Cyclic else TopologicalOrder(finished)
    (forest, result)
  }

  def bfsQueue[Node](start: Node, graph: GRAPH[Node]): Map[Node, List[Node]] = {
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

  // Adjacency list representation of weighted directed graph
  type WGRAPH[Node] = Map[Node, Set[(Double, Node)]]
  
  def dijkstra[Node](start: Node,
    graph: WGRAPH[Node]): Map[Node, (Double, List[Node])] = {
    import scala.collection.mutable.PriorityQueue

    var visited = Map[Node, (Double, List[Node])]()

    implicit val byMinimumDistance =
      Ordering.by((x: (Node, (Double, List[Node]))) => -x._2._1)
    val queue = PriorityQueue[(Node, (Double, List[Node]))]()
    queue.enqueue(start -> (0, Nil))
    while (queue.nonEmpty) {
      queue.dequeue() match {
        case (n, (d, path)) =>
          if (visited contains n) {
            // already visited; ignore
          } else {
            visited += n -> (d, path)
            val path2 = n :: path
            for ((w, n1) <- graph(n)) queue.enqueue(n1 -> (d + w, path2))
          }
      }
    }

    visited
  }

  // Adjacency matrix representation of weighted directed graph
  import scala.collection.mutable.Buffer
  type MGRAPH[Node] = (Buffer[Node], Buffer[Buffer[Double]])
  
  def adjList2Matrix[Node](graph: WGRAPH[Node]): MGRAPH[Node] = {
    val nodes = graph.keys.toBuffer
    val nodeMap = Map((for (i <- 0 until nodes.length) yield nodes(i) -> i): _*)
    val m = Buffer.fill(nodes.length, nodes.length)(Double.PositiveInfinity)
    
    for ((source, neighbors) <- graph) {
      val s = nodeMap(source)
      m(s)(s) = 0
      for ((weight, target) <- neighbors) {
        val t = nodeMap(target)
        m(s)(t) = weight
      }
    }
    
    (nodes, m)
  }
  
  def floyd[Node](graph: MGRAPH[Node]): Unit = {
    val (_, m) = graph
    for (u <- 0 until m.length) {
      for (v <- 0 until m.length) {
        for (w <- 0 until m.length) {
          m(v)(w) = m(v)(w) min (m(v)(u) + m(u)(w))
        }
      }
    }
  }
  
  // TODO Union-find? (Figures 9.19-20)
}
