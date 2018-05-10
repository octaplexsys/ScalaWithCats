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
    @tailrec
    override def tailRecM[A, B](a: A)(nextStep: A => Tree[Either[A, B]]): Tree[B] = {
      nextStep(a) match {
        case Leaf(Right(value)) => Leaf(value)
        case Leaf(Left(leftA)) => tailRecM(leftA)(nextStep)
        case Branch(left: Tree[Either[A, B]], right: Tree[Either[A, B]]) =>

          val leftBranch = flatMap(left)((eitherAorB:Either[A,B]) =>
          eitherAorB match {
            case Left(valueA) => tailRecM(valueA)(nextStep)
            case Right(valueB) => Leaf(valueB)
          })

          val rightBranch = flatMap(right)((eitherAorB:Either[A,B]) =>
            eitherAorB match {
              case Left(valueA) => tailRecM(valueA)(nextStep)
              case Right(valueB) => Leaf(valueB)
            })

          Branch(leftBranch, rightBranch)
      }
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



  def recursive(a: Int): Int = {
    if(a < 100) recursive(a + 1)
    else(a)
  }

  var b = 1
  while(b < 100) {
    b = b + 1
  }

}
