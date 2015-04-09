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

    val forest = aux(graph.keySet)
    val result = if (cycle) Cyclic else TopologicalOrder(finished)
    (forest, result)
  }
  
  // iterative version, with a stack
  def dfsStack[Node](graph: GRAPH[Node]): (FOREST[Node], DFSResult) = {
    import scala.collection.mutable.Stack
    
    var visited = Set[Node]()
    var finished = List[Node]()
    var cycle = false
    
    val roots = graph.keySet.toList map { case n => (None, n) }
    val stack = Stack(roots: _*)
    while (stack.nonEmpty) {
      val (parent, n) = stack.pop()
      if (visited contains n) {
        
      } else {
        
      }
    }
    ???
  }
}

/* from https://www.ics.uci.edu/~eppstein/161/960215.html
    bfs(G)
    {
    list L = empty
    tree T = empty
    choose a starting vertex x
    search(x)
    while(L nonempty)
        remove edge (v,w) from start of L
        if w not yet visited
        {
        add (v,w) to T
        search(w)
        }
    }

    dfs(G)
    {
    list L = empty
    tree T = empty
    choose a starting vertex x
    search(x)
    while(L nonempty)
        remove edge (v,w) from end of L
        if w not yet visited
        {
        add (v,w) to T
        search(w)
        }
    }

    search(vertex v)
    {
    visit(v);
    for each edge (v,w)
        add edge (v,w) to end of L
    }
*/