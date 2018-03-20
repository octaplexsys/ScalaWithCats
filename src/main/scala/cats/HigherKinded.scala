package cats

import cats.implicits._

object HigherKinded extends App {


  val a: Option[String] = Some("test")

  val as: List[String] = List("one", "two")

  def myMethod[F[_], A](aThing: F[A])(implicit F: Functor[F]): F[Unit] =
    F.void(aThing)

  val b: Option[Unit] = myMethod(a)

  val bs: List[Unit] = myMethod(as)

  println(bs.length)
}
