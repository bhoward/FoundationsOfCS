package chapter10

// Based on Figure 10.7 of Aho & Ullman
object BounceFilter {
  sealed abstract class State(val output: Char)
  case object A extends State('0')
  case object B extends State('0')
  case object C extends State('1')
  case object D extends State('1')
  case object Err extends State('X')
  
  def bounce(input: String): String = {
    val states = input.scanLeft(A: State) {
      case (A, '0') => A
      case (A, '1') => B
      
      case (B, '0') => A
      case (B, '1') => C
      
      case (C, '0') => D
      case (C, '1') => C
      
      case (D, '0') => A
      case (D, '1') => C
      
      case (_, _) => Err
    }
    (states.tail map {case s => s.output}).mkString
  }
}
