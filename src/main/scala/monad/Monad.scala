package monad

import scala.language.higherKinds

trait Monad[F[_]] { // Why does this wrap a higher kinded type?
  def pure[A](a: A): F[A]
  def flatMap[A, B](value: A)(f: A => F[B]): F[B]
  def map[A,B](value: A)(f: A => B ): F[B] = {
    flatMap(value)((a: A) => pure(f(a)))
  }

}
