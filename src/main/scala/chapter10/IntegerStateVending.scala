package chapter10

object IntegerStateVending {
  val vtrans = Map[Int, Map[Char, (Int, Boolean)]](
    0 -> Map('N' -> (5, false), 'D' -> (10, false), 'Q' -> (0, true)),
    5 -> Map('N' -> (10, false), 'D' -> (15, false), 'Q' -> (5, true)),
    10 -> Map('N' -> (15, false), 'D' -> (20, false), 'Q' -> (10, true)),
    15 -> Map('N' -> (20, false), 'D' -> (0, true), 'Q' -> (15, true)),
    20 -> Map('N' -> (0, true), 'D' -> (5, true), 'Q' -> (20, true)))

  def vend(input: String) {
    var state = 0 // initial state
    for (coin <- input) {
      val (newState, candy) = vtrans(state)(coin)
      if (candy) println("Candy!")
      state = newState
    }
  }
}
