package monoid

import org.scalatest.{FlatSpec, Matchers}

class SetMonoidUnionTest extends FlatSpec with Matchers {
  "SetMonoidUnion" should "combine associatively" in {
    import SetMonoidUnion._
    val setA = Set("a")
    val setB = Set("a", "b")
    val setC = Set("c")

    combine(combine(setA, setB), setC) shouldBe combine(setA , combine(setB, setC))
  }
  it should "have an identity element" in {
    import SetMonoidUnion._

    val setB = Set("a", "b")
    combine(setB, identity()) shouldBe setB
  }
  "SetMonoidDiff" should "combine associatively" in {
    import SetMonoidDiff._
    val setA = Set("a")
    val setB = Set("a", "b")
    val setC = Set("c")

    combine(combine(setA, setB), setC) shouldBe combine(setA , combine(setB, setC))
  }
  it should "have an identity element" in {
    import SetMonoidDiff._

    val setB = Set("a", "b")
    combine(setB, identity()) shouldBe setB
  }
}
