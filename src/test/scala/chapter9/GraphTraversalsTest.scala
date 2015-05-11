package chapter9

import util.TestSpec

class GraphTraversalsTest extends TestSpec {
  import GraphTraversals._

  val sample1 = Map(
    "BW101" -> Set("BW102", "BW150"),
    "BW102" -> Set("BW150", "BW210", "BW220", "BW230"),
    "BW150" -> Set("BW300"),
    "BW210" -> Set("BW220"),
    "BW220" -> Set("BW230"),
    "BW230" -> Set("BW300", "BW325", "BW350"),
    "BW300" -> Set("BW325", "BW490"),
    "BW325" -> Set("BW350", "BW490"),
    "BW350" -> Set("BW490"),
    "BW490" -> Set[String]())

  val sample2 = sample1 + ("BW350" -> Set("BW300", "BW490"))

  "Recursive Depth-First Search" should "Find a topological sort of a sample graph" in {
    inside(dfs(sample1)) {
      case (_, TopologicalOrder(ns)) =>
        ns.head should be("BW101")
        ns.last should be("BW490")
    }
  }

  it should "Find a cycle in another sample graph" in {
    dfs(sample2)._2 should be(Cyclic)
  }

  "Iterative Depth-First Search" should "Find a topological sort of a sample graph" in {
    inside(dfsStack(sample1)) {
      case (_, TopologicalOrder(ns)) =>
        ns.head should be("BW101")
        ns.last should be("BW490")
    }
  }

  it should "Find a cycle in another sample graph" in {
    dfsStack(sample2)._2 should be(Cyclic)
  }

  "Iterative Breadth-First Search" should "Map nodes to their shortest path from start" in {
    val result = bfsQueue("BW101", sample1)
    result("BW101") should be(Nil)
    result("BW102") should be(List("BW101"))
    result("BW150") should be(List("BW101"))
    result("BW210") should be(List("BW102", "BW101"))
    result("BW220") should be(List("BW102", "BW101"))
    result("BW230") should be(List("BW102", "BW101"))
    result("BW300") should be(List("BW150", "BW101"))
    result("BW325") should (be(List("BW300", "BW150", "BW101")) or be(List("BW230", "BW102", "BW101")))
    result("BW350") should be(List("BW230", "BW102", "BW101"))
    result("BW490") should be(List("BW300", "BW150", "BW101"))
  }

  val wsample = Map(
    "Maili" -> Set((15.0, "Wahiawa"), (20.0, "Pearl City")),
    "Wahiawa" -> Set((15.0, "Maili"), (12.0, "Pearl City"), (28.0, "Laie")),
    "Pearl City" -> Set((20.0, "Maili"), (12.0, "Wahiawa"), (13.0, "Honolulu")),
    "Laie" -> Set((28.0, "Wahiawa"), (24.0, "Kaneohe")),
    "Kaneohe" -> Set((24.0, "Laie"), (11.0, "Honolulu")),
    "Honolulu" -> Set((13.0, "Pearl City"), (11.0, "Kaneohe")))
    
  "Dijkstra's Algorithm" should "Map nodes to their distance and shortest path from start" in {
    val result = dijkstra("Honolulu", wsample)
    result("Honolulu") should be(0 -> Nil)
    result("Pearl City") should be(13 -> List("Honolulu"))
    result("Maili") should be(33 -> List("Pearl City", "Honolulu"))
    result("Wahiawa") should be(25 -> List("Pearl City", "Honolulu"))
    result("Laie") should be(35 -> List("Kaneohe", "Honolulu"))
    result("Kaneohe") should be(11 -> List("Honolulu"))
  }
  
  "Floyd's Algorithm" should "Find shortest distances between all pairs of nodes" in {
    val msample = adjList2Matrix(wsample)
    floyd(msample)
    
    val (nodes, m) = msample
    val nodeMap = Map((for (i <- 0 until nodes.length) yield nodes(i) -> i): _*)

    def dist(a: String, b: String): Double = m(nodeMap(a))(nodeMap(b))
    
    dist("Honolulu", "Honolulu") should be(0)
    dist("Honolulu", "Kaneohe") should be(11)
    dist("Honolulu", "Pearl City") should be(13)
    dist("Honolulu", "Wahiawa") should be(25)
    dist("Honolulu", "Laie") should be(35)
    dist("Honolulu", "Maili") should be(33)
    dist("Kaneohe", "Honolulu") should be(11)
    dist("Kaneohe", "Kaneohe") should be(0)
    dist("Kaneohe", "Pearl City") should be(24)
    dist("Kaneohe", "Wahiawa") should be(36)
    dist("Kaneohe", "Laie") should be(24)
    dist("Kaneohe", "Maili") should be(44)
    dist("Pearl City", "Honolulu") should be(13)
    dist("Pearl City", "Kaneohe") should be(24)
    dist("Pearl City", "Pearl City") should be(0)
    dist("Pearl City", "Wahiawa") should be(12)
    dist("Pearl City", "Laie") should be(40)
    dist("Pearl City", "Maili") should be(20)
    dist("Wahiawa", "Honolulu") should be(25)
    dist("Wahiawa", "Kaneohe") should be(36)
    dist("Wahiawa", "Pearl City") should be(12)
    dist("Wahiawa", "Wahiawa") should be(0)
    dist("Wahiawa", "Laie") should be(28)
    dist("Wahiawa", "Maili") should be(15)
    dist("Laie", "Honolulu") should be(35)
    dist("Laie", "Kaneohe") should be(24)
    dist("Laie", "Pearl City") should be(40)
    dist("Laie", "Wahiawa") should be(28)
    dist("Laie", "Laie") should be(0)
    dist("Laie", "Maili") should be(43)
    dist("Maili", "Honolulu") should be(33)
    dist("Maili", "Kaneohe") should be(44)
    dist("Maili", "Pearl City") should be(20)
    dist("Maili", "Wahiawa") should be(15)
    dist("Maili", "Laie") should be(43)
    dist("Maili", "Maili") should be(0)
  }
  
  // TODO write more tests, and some properties
}
