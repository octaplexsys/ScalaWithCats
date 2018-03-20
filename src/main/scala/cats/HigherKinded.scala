package cats

import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

object HigherKinded extends App {
  val a: Option[String] = Some("test")

  val as: List[String] = List("one", "two")

  def myMethod[F[_], A](aThing: F[A])(implicit F: Functor[F]): F[Unit] =
    F.void(aThing)

  val b: Option[Unit] = myMethod(a)

  val bs: List[Unit] = myMethod(as)

  println(bs.length)

  // The type of this is flatMap[B](f: A => Option[Unit])
  Some(1).flatMap(_ => Some(Unit))
  // The type of this is flatMap[B](f: A => List[Unit])
  List(1).flatMap(_ => List(Unit))
  // The type of this is flatMap[B](f: A => Future[Unit])
  Future.successful("HI").flatMap(_ => Future.successful(Unit))(ExecutionContext.global)

  // These are all really similar so...
  trait FlatMap2Unit[F[_]]{ // this is flatmap (aka monad) with second type fixed to unit
    def mapAThingToUnit[A](instance: F[A], f: A => F[Unit]): F[Unit]
  }

  object instancesForUnitMapper {

    implicit val optionMapper: FlatMap2Unit[Option] = new FlatMap2Unit[Option] {
      override def mapAThingToUnit[A](instance: Option[A], f: A => Option[Unit]): Option[Unit] = instance flatMap f
    }

    implicit val listMapper: FlatMap2Unit[List] = new FlatMap2Unit[List] {
      override def mapAThingToUnit[A](instance: List[A], f: A => List[Unit]): List[Unit] = instance flatMap f
    }

    implicit def futureMapper(implicit ec: ExecutionContext): FlatMap2Unit[Future] = new FlatMap2Unit[Future] {
      override def mapAThingToUnit[A](instance: Future[A], f: A => Future[Unit]): Future[Unit] = instance flatMap f
    }
  }
}
