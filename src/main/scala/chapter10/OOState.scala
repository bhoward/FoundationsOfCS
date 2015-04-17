package chapter10

object OOState {
  def testWord(word: String): Boolean = {
    trait State {
      def trans(c: Char): State
      def accept: Boolean = false // override to identify an accepting state
    }

    object InitialState extends State {
      def trans(c: Char) = if (c == 'a') AState else InitialState
    }

    object AState extends State {
      def trans(c: Char) = if (c == 'e') AEState else AState
    }

    object AEState extends State {
      def trans(c: Char) = if (c == 'i') AEIState else AEState
    }

    object AEIState extends State {
      def trans(c: Char) = if (c == 'o') AEIOState else AEIState
    }

    object AEIOState extends State {
      def trans(c: Char) = if (c == 'u') AEIOUState else AEIOState
    }

    object AEIOUState extends State {
      def trans(c: Char) = AEIOUState
      override def accept = true
    }

    var state: State = InitialState
    for (c <- word) {
      state = state.trans(c)
    }
    state.accept
  }
}
