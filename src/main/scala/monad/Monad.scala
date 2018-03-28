package monad

import scala.language.higherKinds

trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](value: A)(f: A => F[B]): F[B]
  def map[A,B](value: A)(f: A => B ): F[B] = {
    flatMap(value)((a: A) => pure(f(a)))
  }

}
