object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println()

    List[String](
      "(if (zero? x) max (/ 1 x))",
      "I told him (that it's not (yet) done). (But he wasn't listening)",
      ":-)",
      "())("
    ).foreach((it: String) => println(s"$it is balanced: ${balance(it.toList)}"))

    println()

    Map(
      0 -> List[Int](1),
      1 -> List[Int](0),
      1 -> List[Int](),
      1 -> List[Int](1),
      2 -> List[Int](1),
      2 -> List[Int](1, 2),
      3 -> List[Int](1),
      3 -> List[Int](1, 2),
      3 -> List[Int](1, 2, 3),
      4 -> List[Int](1, 2),
      5 -> List[Int](1, 2, 5),
      10 -> List[Int](1, 2, 5),
      12 -> List[Int](1, 2, 5),
      12 -> List[Int](3, 5)
    ).foreach((t: (Int, List[Int])) => {
        val i = t._1
        val denoms = t._2
        println(s"$i can be exchanged in ${countChange(i, denoms)} ways with denominations $denoms")
      }
    )
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(c <= 0 || c >= r || r < 0) {
      return 1
    } else {
      return pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  private val openingPattern = """(.*)\((.*?)""".r
  private val closingPattern = """(.*?)\)(.*)""".r
  def balance(chars: List[Char]): Boolean = {
    chars.mkString match {
      case openingPattern(left_outside, innards) => innards match {
        case closingPattern(_, right_outside) => balance((left_outside + right_outside).toList)
        case _ => false
      }
      case closingPattern(_*) => false
      case _ => true
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else if(money < 0 || coins.forall((coin: Int) => money < coin)) 0
    else coins.map((coin: Int) => countChange(money - coin, coins.filter((c: Int) => c <= coin))).sum
  }
}
