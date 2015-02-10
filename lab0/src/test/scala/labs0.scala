import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._

class Lab0Spec extends FunSpec with Matchers {

  describe("The pascal function") {

    val binomials = Table(

        ("n", "k", "b"),
        (  0,   0,   1),
        (  0,   1,   1),
        (  0,   4,   1),

        (  1,   0,   1),
        (  1,   1,   1),

        (  2,   0,   1),
        (  2,   1,   2),
        (  2,   2,   1),

        (  3,   0,   1),
        (  3,   1,   3),
        (  3,   2,   3),
        (  3,   3,   1),

        (  4,   0,   1),
        (  4,   1,   4),
        (  4,   2,   6),
        (  4,   3,   4),
        (  4,   4,   1),

        (  0, 100,   1)

    )

    forAll(binomials) { (r: Int, c: Int, b: Int) =>
      it(s"returns $b for ($c, $r)") {
        Main.pascal(c, r) should be (b)
      }
    }
  }

  describe("The balance function") {

    val balances = Table(

      ("isBalanced", "string"),
      (true,  "(if (zero? x) max (/ 1 x))"),
      (true,  "I told him (that it's not (yet) done). (But he wasn't listening)"),
      (false, ":-)"),
      (false, "())(")

    )

    forAll(balances) { (isBalanced: Boolean, string: String) =>
      it(s"returns $isBalanced for: $string") {
        Main.balance(string.toList) should be (isBalanced)
      }
    }
  }

  describe("The countChange function") {
    val tests = Table(
      ("amount", "denominations", "expectedResult") ,
      ( 0, List(1),        1),
      ( 1, List(0),        0),
      ( 1, List(),         0),
      ( 1, List(1),        1),
      ( 2, List(1),        1),
      ( 2, List(1, 2),     2),
      ( 3, List(1),        1),
      ( 3, List(1, 2),     2),
      ( 3, List(1, 2, 3),  3),
      ( 4, List(1, 2),     3),
      ( 5, List(1, 2, 5),  4),
      (10, List(1, 2, 5), 10),
      (12, List(1, 2, 5), 13),
      (12, List(3, 5),     1),
      (12, List(7),        0)
    )

    forAll(tests) { (amount: Int, denominations: List[Int], expectedResult: Int) =>
      it(s"returns $expectedResult for amount=$amount, denominations=$denominations") {
        Main.countChange(amount, denominations) should be (expectedResult)
      }
    }
  }

}
