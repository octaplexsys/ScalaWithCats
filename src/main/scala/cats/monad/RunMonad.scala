package cats.monad

import cats.Monad
import cats.instances.option._
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.Id

object RunMonad extends App {
  println(Monad[Option].pure(1).map(_ + 1))// this map is the regular option map.......
  println(Monad[List].pure("jfkdf"))// list and option already form a monad, why do I need this?


  def sumSquare[F[_]](a: F[Long], b: F[Long])(implicit m: Monad[F]): F[Long] =
    m.map2(a,b)(sumSquares)

  def sumSquareForComprehension[F[_]](a: F[Long], b: F[Long])(implicit m: Monad[F]):F[Long] =
    for{
      x <- a
      y <- b
    } yield sumSquares(x, y)

  private def sumSquares(a: Long, b: Long): Long =
    a * a + b * b

  println(sumSquare(Option(3L), Option(1L)))
  println(sumSquare(List(1L,2L), List(3L,4L)))

  println(sumSquareForComprehension(Option(3L), Option(1L)))
  println(sumSquareForComprehension(List(1L,2L), List(3L,4L)))

  println(sumSquare[Id](45L, 59993721L))
}
