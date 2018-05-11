package cats.functor

import cats.{Functor, Monad}

import scala.annotation.tailrec

sealed trait Tree[+T]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree{
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(me: A => B): Tree[B] = {
      fa match {
        case Leaf(alone) => Leaf(me(alone))
        case Branch(left, right) => Branch(map(left)(me),map(right)(me))
      }
    }
  }

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      case Leaf(value) => f(value)
    }

    // WAT IS IT DOING????
        def thisIsNotTailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
          f(a) match {
            case Leaf(Right(value)) => Leaf(value)
            case Leaf(Left(leftA)) => tailRecM(leftA)(f)
            case Branch(left: Tree[Either[A, B]], right: Tree[Either[A, B]]) =>

              val leftBranch = flatMap(left)((eitherAorB:Either[A,B]) =>
              eitherAorB match {
                case Left(valueA) => tailRecM(valueA)(f)
                case Right(valueB) => Leaf(valueB)
              })

              val rightBranch = flatMap(right)((eitherAorB:Either[A,B]) =>
                eitherAorB match {
                  case Left(valueA) => tailRecM(valueA)(f)
                  case Right(valueB) => Leaf(valueB)
                })

              Branch(leftBranch, rightBranch)
          }
        }

    // Actual tail recursive implementation
    override def tailRecM[A, B](a: A)(f: (A) => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def go(toVisit: List[Tree[Either[A, B]]],
             toCollect: List[Tree[B]]): List[Tree[B]] = toVisit match {
        case (tree :: tail) =>
          tree match {
            case Branch(l, r) =>
              l match {
                case Branch(_, _) => go(l :: r :: tail, toCollect)
                case Leaf(Left(a)) => go(f(a) :: r :: tail, toCollect)
                case Leaf(Right(b)) => go(r :: tail, Leaf(b) +: toCollect)
              }
            case Leaf(Left(a)) => go(f(a) :: tail, toCollect)
            case Leaf(Right(b)) =>
              go(tail,
                if (toCollect.isEmpty) Leaf(b) +: toCollect
                else Branch(toCollect.head, Leaf(b)) :: toCollect.tail)
          }
        case Nil => toCollect
      }

      go(f(a) :: Nil, Nil).head
    }
  }

}

object TreeMain extends App {
  import Tree._
  import cats.syntax.functor._
  import cats.syntax.monad._
  val tree: Tree[String] = Branch(Leaf("HI"),Branch(Leaf("PLZ"),Leaf("WORK")))

  val test = Functor[Tree].map(tree)(v => addExclamations(v))
  tree.map[String](addExclamations)

  def addExclamations(str: String): String = {
    str + "!!"
  }

  val fmTree = Monad[Tree].flatMap(tree)(s => {
    Branch(Leaf(s.head.toString), Leaf(s.tail))
  })
  println(fmTree)


  def walkTreeDFS(a: Tree[String]): String =
    a match {
      case Leaf(s) => s
      case Branch(l, _) =>
        walkTreeDFS(l)
    }

  def walkTreeDFSM(a: Tree[String]): String =
    Monad[Tree].tailRecM(a) {
      case Leaf(s) => Leaf(Right(s))
      case Branch(l, _) => Leaf(Left(l))
    } match {
      case Leaf(s) => s
    }

  // Example use case: given a string, take things out of the string and build a tree.
  def stringToTree(inputString: String): Tree[String] = {
    val rows = inputString.split("\n").toList
    walkTreeByRow(rows, null)
  }

  def walkTreeByRow(rows: List[String], treeSoFar: Tree[String]): Tree[String] = {
    rows match {
      case row::rowsToVisit => walkRow(row, rowsToVisit, treeSoFar: Tree[String]) // given a row, walk it to create a tree
      case Nil => println("end of rows " + treeSoFar); treeSoFar // when you reach the end, return your tree that you created
    }
  }

  def walkRow(s: String, remainderOfRows: List[String], treeSoFar: Tree[String]): Tree[String] = {
    s.toList match {
      case Nil => treeSoFar
      case h :: tail if h != '^' => println(s"the head is $h; the tail is $tail"); walkRow(tail.toString, remainderOfRows, Leaf(h.toString)) // here, we are finally making the tree, the tree so far is a Leaf
      case h :: tail if h == '^' => Branch(walkTreeByRow(remainderOfRows, treeSoFar), walkRow(tail.toString, remainderOfRows, treeSoFar)) // This is so broken it's not even funny
    }
  }

  val tallerTree =
    """
          ^
        ^  c
       a b
    """.replaceAll(" ", "")
  println(tallerTree)
}
