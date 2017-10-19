package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r - c == 0) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char], count: Int = 0): Boolean = {
    if (count < 0) false
    else if (chars.isEmpty) count == 0
    else chars.head match {
      case '(' => balance(chars.tail, count + 1)
      case ')' => balance(chars.tail, count - 1)
      case _ => balance(chars.tail, count)
    }
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sortedCoins = coins.sorted.toArray

    def howMany(x: Int, index: Int): Int = {
      if (x < 0 || index < 0) 0
      else if (x < sortedCoins(index)) howMany(x, index - 1)
      else if (x == sortedCoins(index)) 1 + howMany(x, index - 1)
      else {
        val r = x - sortedCoins(index)
        howMany(r, index) + howMany(x, index - 1)
      }
    }

    howMany(money, sortedCoins.length - 1)
  }
}
