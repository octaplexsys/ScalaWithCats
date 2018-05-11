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
    "make a well formed tree" in {
      val tallerTree =
        """
          ^
        c  ^
       a b
    """.replaceAll(" ", "")
      stringToTree(tallerTree) shouldBe Branch(Leaf('c'),Branch(Leaf('a'),Leaf('b')))
    }
    "make a taaaall tree" in {
      val evenTallerTree =
        """
          ^
        ^  ^
       a b c ^
             d e
    """.replaceAll(" ", "")
      stringToTree(evenTallerTree) shouldBe Branch(Branch(Leaf('a'), Leaf('b')),Branch(Leaf('c'), Branch(Leaf('d'), Leaf('e'))))
    }
  }

  "treeToList" should {
    "make a list from a single leaf" in {
      treeToList(Leaf("a")) shouldBe List("a")
    }
    "make a list from a single branch" in {
      treeToList(Branch(Leaf("a"), Leaf("b"))) shouldBe List("a", "b")
    }
    "make a list from a taller tree" in {
      val tallTree = Branch(Branch(Leaf("a"), Leaf("b")),Branch(Leaf("c"), Branch(Leaf("d"), Leaf("e"))))
      treeToList(tallTree) shouldBe List("a","b","c","d","e")
    }
  }

  "treeToListTailRecM" should {
    "make a list from a single leaf" in {
      treeToListTailRecM(Leaf("a")) shouldBe List("a")
    }
    "make a list from a single branch" in {
      treeToListTailRecM(Branch(Leaf("a"), Leaf("b"))) shouldBe List("a", "b")
    }
    "make a list from a taller tree" in {
      val tallTree = Branch(Branch(Leaf("a"), Leaf("b")),Branch(Leaf("c"), Branch(Leaf("d"), Leaf("e"))))
      treeToListTailRecM(tallTree) shouldBe List("a","b","c","d","e")
    }
  }
}


