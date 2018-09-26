import ScalaGoose._
import org.scalatest.FunSuite

class TestSuite extends FunSuite {
  test("dice between 1 and 6") {
    val dice = new dice
    val diceNumber= dice.launch
    assert(diceNumber<7 & diceNumber>0)
  }
}
