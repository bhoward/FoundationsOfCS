package chapter5

import util.TestSpec

class GeneralTreeTest extends TestSpec {
  import GeneralTree._

  val leaf = Tree("Single leaf", Nil)

  val t1 =
    Tree("root", List(
      Tree("child1", List(
        Tree("child11", Nil),
        Tree("child12", Nil))),
      Tree("child2", Nil),
      Tree("child3", List(
        Tree("child31", Nil),
        Tree("child32", List(
          Tree("child321", Nil),
          Tree("child322", Nil))),
        Tree("child33", Nil)))))

  "PreOrder traversal on general trees" should "work on single leaves" in {
    val expected =
      """Single leaf
        |""".stripMargin
    outputFrom(preOrder(leaf)) should equal(expected)
  }

  it should "work on a sample tree" in {
    val expected =
      """root
        |child1
        |child11
        |child12
        |child2
        |child3
        |child31
        |child32
        |child321
        |child322
        |child33
        |""".stripMargin
    outputFrom(preOrder(t1)) should equal(expected)
  }

  "PostOrder traversal on general trees" should "work on single leaves" in {
    val expected =
      """Single leaf
        |""".stripMargin
    outputFrom(postOrder(leaf)) should equal(expected)
  }

  it should "work on a sample tree" in {
    val expected =
      """child11
        |child12
        |child1
        |child2
        |child31
        |child321
        |child322
        |child32
        |child33
        |child3
        |root
        |""".stripMargin
    outputFrom(postOrder(t1)) should equal(expected)
  }

  "Expression tree evaluation" should "work on a number" in {
    val e = Tree("42", Nil)
    eval(e) should equal(42)
  }

  it should "work on a simple expression" in {
    val e = Tree("*", List(Tree("6", Nil), Tree("7", Nil)))
    eval(e) should equal(42)
  }

  it should "work on a complex expression" in {
    val e1 = Tree("+", List(Tree("1", Nil), Tree("2", Nil)))
    val e2 = Tree("-", List(Tree("24", Nil), Tree("3", Nil)))
    val e3 = Tree("/", List(e2, e1))
    val e4 = Tree("*", List(e3, Tree("+", List(e1, Tree("5", Nil)))))
    eval(e4) should equal(56)
  }

  "Height" should "work on single leaves" in {
    height(leaf) should equal(0)
  }

  it should "work on a sample tree" in {
    height(t1) should equal(3)
  }
}
