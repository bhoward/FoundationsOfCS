package chapter10

object IntegerState {
  // Graph of transitions
  val trans = Array[Map[Char, Int]](
    Map('a' -> 1) withDefaultValue 0, // state 0
    Map('e' -> 2) withDefaultValue 1, // state 1
    Map('i' -> 3) withDefaultValue 2, // state 2
    Map('o' -> 4) withDefaultValue 3, // state 3
    Map('u' -> 5) withDefaultValue 4, // state 4
    Map() withDefaultValue 5 // state 5
    )

  def testWord(word: String): Boolean = {
    var state = 0 // initial state
    for (c <- word) {
      state = trans(state)(c)
    }
    state == 5 // returns true if accepting state reached
  }
}
