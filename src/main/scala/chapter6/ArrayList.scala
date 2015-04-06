package chapter6

// Based on Figures 6.11, 6.12, and 6.14 of Aho & Ullman
// Instead of a fixed maximum size, this list will grow as needed
object ArrayList {
  val INITIAL_SIZE = 10

  class LIST {
    // Invariant: length < A.length, so A(A.length - 1) is always empty;
    //   data is kept in locations A(0) through A(length - 1)
    private var A: Array[Int] = new Array[Int](INITIAL_SIZE)
    private var length: Int = 0

    private def addOneToLength: Unit = {
      length += 1
      if (length == A.length) {
        val newA = new Array[Int](A.length * 2)
        A.copyToArray(newA)
        A = newA
      }
    }

    // Add x at end of array
    def insert(x: Int): Unit = {
      A(length) = x
      addOneToLength
    }

    // Linear Search -- Figure 6.11
    def lookup(x: Int): Boolean = {
      for (i <- 0 until length) {
        if (x == A(i)) return true // scalastyle:ignore
      }
      false
    }

    // Linear Search with Sentinel -- Figure 6.12
    def lookup2(x: Int): Boolean = {
      A(length) = x // temporarily use the next empty space
      var i = 0
      while (x != A(i)) {
        i += 1
      }
      i < length // true if found before reaching sentinel value
    }

    // Add x in correct sorted position
    // Precondition: A(0) to A(length - 1) are in non-decreasing order
    // Postcondition: length is increased by one, x is included in elements,
    //   and A(0) to A(length - 1) are in non-decreasing order
    def insertSorted(x: Int): Unit = {
      var i = length
      while (i > 0 && A(i - 1) > x) {
        A(i) = A(i - 1)
        i -= 1
      }
      // now either i == 0 or A(i - 1) <= x,
      // and either i == length or A(i + 1) > x
      A(i) = x
      addOneToLength
    }

    // Binary search for x -- Figure 6.14
    // Precondition: A(0) to A(length - 1) are in non-decreasing order
    def binsearch(x: Int): Boolean = {
      def aux(low: Int, high: Int): Boolean = {
        if (low > high) { // not found
          false
        } else {
          val mid = low + (high - low) / 2
          if (x < A(mid)) {
            aux(low, mid - 1)
          } else if (x > A(mid)) {
            aux(mid + 1, high)
          } else { // x == A(mid)
            true
          }
        }
      }

      aux(0, length - 1)
    }
  }
}
