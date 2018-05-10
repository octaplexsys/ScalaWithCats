package monad

import org.scalatest.{Matchers, WordSpec}

class CalculatorTest extends WordSpec with Matchers {
  import Calculator._
  "evalAll" should {
    "take in a number and put it on the stack" in {
      evalAll("5").run(List()) shouldBe (List(5), ())
    }
    "take in two numbers and place them both on the stack" in {
      evalAll("1,2").run(List()) shouldBe (List(2,1), ())
    }
    "take in two numbers and an operator and perform the evaluation" in {
      evalAll("1,2,+").run(List()) shouldBe (List(3), ())
    }
    "take in multiple numbers and operators" in {
      evalAll("0,2,*,1,+,5,*").run(List()) shouldBe (List(5),())
    }
  }
}
