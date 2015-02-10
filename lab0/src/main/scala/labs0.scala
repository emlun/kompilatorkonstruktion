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
  def pascal(c: Int, r: Int): Int =
    if(c <= 0 || c >= r || r < 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  private val openingPattern = """(.*)\((.*?)""".r
  private val closingPattern = """(.*?)\)(.*)""".r
  def balance(chars: List[Char]): Boolean =
    chars.mkString match {
      case openingPattern(left_outside, innards) => innards match {
        case closingPattern(_, right_outside) => balance((left_outside + right_outside).toList)
        case _ => false
      }
      case closingPattern(_*) => false
      case _ => true
    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if(coins.isEmpty || coins.max <= 0) 0
    else countChangeInternal(money, coins)

  private def countChangeInternal(money: Int, coins: List[Int]): Int =
    if(money == 0) 1
    else if(money < 0 || coins.forall((coin: Int) => money < coin)) 0
    else coins.map((coin: Int) => countChange(money - coin, coins.filter((c: Int) => c <= coin))).sum
}
