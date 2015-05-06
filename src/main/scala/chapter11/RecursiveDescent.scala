package chapter11

// Based on Figure 11.27 of Aho & Ullman
object RecursiveDescent {
  case class TREE(label: Char, children: List[TREE])

  // Production rules for B:
  // (1) B --> epsilon
  // (2) B --> ( B ) B
  def B(input: String): Option[(TREE, String)] = {
    if (input.nonEmpty && input.head == '(') {
      // follow production 2
      B(input.tail) match {
        case Some((firstB, input2)) =>
          if (input2.nonEmpty && input2.head == ')') {
            B(input2.tail) match {
              case Some((secondB, input3)) =>
                val t = TREE('B', List(
                  TREE('(', Nil),
                  firstB,
                  TREE(')', Nil),
                  secondB))
                Some((t, input3))
              case None =>
                // second call to B failed
                None
            }
          } else {
            None
          }
        case None =>
          // first call to B failed
          None
      }
    } else {
      // follow production 1
      val t = TREE('e', Nil)
      Some((t, input))
    }
  }
  
  def main(args: Array[String]): Unit = {
    val in = new java.util.Scanner(System.in)
    while (in.hasNextLine) {
      val line = in.nextLine()
      B(line) match {
        case Some((t, leftover)) =>
          println("Success:")
          printTree(t)
          println("Unparsed input: " + leftover)
          println()
        case None =>
          println("Fail")
      }
    }
  }
  
  def printTree(t: TREE, indent: String = ""): Unit = t match {
    case TREE(label, children) =>
      println(indent + label)
      for (child <- children) {
        printTree(child, indent + "  ")
      }
  }
}