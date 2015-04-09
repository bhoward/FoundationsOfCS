package chapter9

// Based on Figures 9.27, 9.31, 9.33, and 9.44-47 of Aho & Ullman
object GraphTraversals {
  // Adjacency list representation of directed labeled graph
  type GRAPH[Node, Edge] = Map[Node, Set[(Edge, Node)]]

  case class TREE[Node](root: Node, children: FOREST[Node])
  type FOREST[Node] = List[TREE[Node]]

  // recursive version:
  def dfs[Node, Edge](node: Node,
                      graph: GRAPH[Node, Edge],
                      visited: Set[Node]): (TREE[Node], Set[Node]) = {
    val (children, visited2) = graph(node).foldLeft((Nil: FOREST[Node], visited + node)) {
      case ((ts, vs), (_, n)) =>
        if (vs contains n) {
          (ts, vs)
        } else {
          val (t, vs2) = dfs(n, graph, vs)
          (t :: ts, vs2)
        }
    }
    (TREE(node, children), visited2)
  }
  
  // TODO there's a lot of overlap here...
  // -- write common function that takes set of nodes and returns dfs forest
  def dfsForest[Node, Edge](graph: GRAPH[Node, Edge]): FOREST[Node] = {
    val (forest, _) = graph.keySet.foldLeft((Nil: FOREST[Node], Set[Node]())) {
      case ((ts, vs), n) =>
        if (vs contains n) {
          (ts, vs)
        } else {
          val (t, vs2) = dfs(n, graph, vs)
          (t :: ts, vs2)
        }
    }
    forest
  }
}
