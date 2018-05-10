package cats.functor

import cats.{Functor, Monad}

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

  val treeMonad = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      case Leaf(value) => f(value)
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = ???
  }
}

object TreeMain extends App {
  import Tree._
  import cats.syntax.functor._
  val tree: Tree[String] = Branch(Leaf("HI"),Branch(Leaf("PLZ"),Leaf("WORK")))

  val test = Functor[Tree].map(tree)(v => addExclamations(v))
  tree.map[String](addExclamations)

  def addExclamations(str: String): String = {
    str + "!!"
  }

  println(test)
}
