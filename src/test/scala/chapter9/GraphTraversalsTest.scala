package chapter9

import util.TestSpec

class GraphTraversalsTest extends TestSpec {
  import GraphTraversals._

  val sample1: GRAPH[String] = Map(
    "BW101" -> Set("BW102", "BW150"),
    "BW102" -> Set("BW150", "BW210", "BW220", "BW230"),
    "BW150" -> Set("BW300"),
    "BW210" -> Set("BW220"),
    "BW220" -> Set("BW230"),
    "BW230" -> Set("BW300", "BW325", "BW350"),
    "BW300" -> Set("BW325", "BW490"),
    "BW325" -> Set("BW350", "BW490"),
    "BW350" -> Set("BW490"),
    "BW490" -> Set())

  val sample2 = sample1 + ("BW350" -> Set("BW300", "BW490"))

  "Depth-First Search" should "Find a topological sort of a sample graph" in {
    inside(dfs(sample1)) {
      case (_, TopologicalOrder(ns)) =>
        ns.head should be("BW101")
        ns.last should be("BW490")
    }
  }

  it should "Find a cycle in another sample graph" in {
    dfs(sample2)._2 should be(Cyclic)
  }
  
  // TODO write more tests, and some properties
}