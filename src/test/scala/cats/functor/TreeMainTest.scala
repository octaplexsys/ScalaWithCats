package cats.functor

import org.scalatest.{Matchers, WordSpec}

class TreeMainTest extends WordSpec with Matchers {
  import TreeMain._
  "stringToTree" should {
    "take in a leaf branch and turn it into a tree" in {
      val leafyTree = "a"
      stringToTree(leafyTree) shouldBe Leaf('a')
    }
    "take in a tree with depth of one" in {
      val branchyTree =
        """ ^
           a b
        """.replaceAll(" ", "")
      stringToTree(branchyTree) shouldBe Branch(Leaf('a'),Leaf('b'))
    }
    "take in a tree with depth of two" in {
      val tallerTree =
        """
          ^
        ^  c
       a b
    """.replaceAll(" ", "")
      stringToTree(tallerTree) shouldBe Branch(Branch(Leaf('a'),Leaf('b')), Leaf('c'))
    }
  }
}


