package cats.monad

import cats.instances.future._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Id, Monad}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object RunMonad extends App {

  // cats monad instances
  println(Monad[Option].pure(1).map(_ + 1))
  println(Monad[List].pure("jfkdf"))
  val fm = Monad[Future]
  fm.pure("hello").flatMap(x => Future(x + "!!"))

  import SumSquare._

  println(sumSquare(Option(3L), Option(1L)))
  println(sumSquare(List(1L,2L), List(3L,4L)))

  println(sumSquareForComprehension(Option(3L), Option(1L)))
  println(sumSquareForComprehension(List(1L,2L), List(3L,4L)))

  println(sumSquare[Id](45L, 59993721L))


}

object SumSquare{
  def sumSquare[F[_]](a: F[Long], b: F[Long])(implicit m: Monad[F]): F[Long] =
    m.map2(a,b)(sumSquares)

  def sumSquareForComprehension[F[_]](a: F[Long], b: F[Long])(implicit m: Monad[F]):F[Long] =
    for{
      x <- a
      y <- b
    } yield sumSquares(x, y)

  private def sumSquares(a: Long, b: Long): Long =
    a * a + b * b
}

object CatsEither extends App {
import cats.syntax.either._
  val e1: Either[Nothing, Int] = Right(1)
  val e2 = Right(3)
  val result = for {
    a <- e1
    b <- e2
  } yield a + b
  println(result)

  val a: Either[String, Int] = 3.asRight[String] // Nice syntax as it returns either instead of l/r
  val b = 6.asRight[String]
  for {
    x <- a
    y <- b
  } yield x + y

  def countPositives(nums: List[Int]): Either[String,Int] = {
    nums.foldLeft(0.asRight[String])((acc, next) =>
      if (next >=0) acc.map(_ + 1 )
      else Left("Negative number fount")
    )
  }

  countPositives(List(1,2,3))
  countPositives(List(1,-2,3))

}