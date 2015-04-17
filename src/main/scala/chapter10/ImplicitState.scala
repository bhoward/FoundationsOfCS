package chapter10

// Based on Figure 10.2 of Aho & Ullman
object ImplicitState {
  def testWord(word: String): Boolean = {
    var index = 0

    /**
     * Advance index through the characters of word until either
     * c is found (return true; index points at character after c), or
     * the end of the string is reached (return false).
     */
    def findChar(c: Char): Boolean = {
      while (index < word.length && word(index) != c) {
        index += 1
      }

      if (index == word.length) {
        false
      } else {
        index += 1
        true
      }
    }

    // state 0
    if (findChar('a'))
      // state 1
      if (findChar('e'))
        // state 2
        if (findChar('i'))
          // state 3
          if (findChar('o'))
            // state 4
            if (findChar('u'))
              // state 5
              return true
    // error state
    false
  }
}
