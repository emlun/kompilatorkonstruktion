import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.TableDrivenPropertyChecks._

@RunWith(classOf[JUnitRunner])
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

}
