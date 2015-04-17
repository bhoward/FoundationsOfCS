package chapter10

object OOStateVending {
  class VendingMachine(price: Int) {
    private var balance = 0

    /**
     * Insert the given amount of money; returns true if an item is vended.
     */
    def deposit(amount: Int): Boolean = {
      balance += amount
      if (balance >= price) {
        balance -= price
        true
      } else {
        false
      }
    }
  }

  def vend(input: String) {
    val machine = new VendingMachine(25)
    for (coin <- input) {
      val candy = coin match {
        case 'N' => machine.deposit(5)
        case 'D' => machine.deposit(10)
        case 'Q' => machine.deposit(25)
      }
      if (candy) println("Candy!")
    }
  }
}
