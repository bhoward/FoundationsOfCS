package chapter5

// Based on Figures 5.15, 5.18, 5.19, and 5.22 of Aho & Ullman
object GeneralTree {
  case class Tree(label: String, children: List[Tree])

  def preOrder(t: Tree): Unit = {
    println(t.label)
    for (child <- t.children) {
      preOrder(child)
    }
  }

  def postOrder(t: Tree): Unit = {
    for (child <- t.children) {
      postOrder(child)
    }
    println(t.label)
  }

  def eval(t: Tree): Int = {
    if (t.children.isEmpty) {
      // t is a leaf -- label contains a numeral
      t.label.toInt
    } else {
      // t is an interior node -- label is an operator
      val left = eval(t.children.head)
      val right = eval(t.children.tail.head)
      t.label match {
        case "+" => left + right
        case "-" => left - right
        case "*" => left * right
        case "/" => left / right
      }
    }
  }

  def height(t: Tree): Int = {
    if (t.children.isEmpty) {
      0
    } else {
      1 + (t.children map height).max
    }
  }
}
