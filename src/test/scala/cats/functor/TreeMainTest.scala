package cats.functor

import org.scalatest.{Matchers, WordSpec}

class TreeMainTest extends WordSpec with Matchers {
  import TreeMain._




  "stringToTree" should {
    "take in a leaf branch and turn it into a tree" in {
      val leafValue = "a"
      stringToTree(leafValue) shouldBe Leaf(leafValue)
    }
    "take in a tree with depth of one" in {
      val branchyTree =
        """ ^
           a b
        """.stripMargin
      stringToTree(branchyTree) shouldBe Branch(Leaf("a"),Leaf("b"))
    }
    "take in a tree with depth of two" in {
      val testTree =
        """
          ^
        ^  c
       a b
    """.stripMargin
      stringToTree(testTree) shouldBe Branch(Branch(Leaf("a"),Leaf("b")), Leaf("c"))
    }
  }
}
