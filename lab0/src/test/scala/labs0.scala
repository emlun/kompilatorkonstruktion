import org.scalatest._

class Lab0Spec extends FlatSpec with Matchers {

  "The pascal function" should "return 1 for (0, 4)" in {
    Main.pascal(0, 4) should be (1)
  }

}
