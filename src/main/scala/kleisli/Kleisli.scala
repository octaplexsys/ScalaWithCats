package kleisli

import cats.FlatMap

// Kleisli is a way of composing functions in a wrapped context.
trait Kleisli[F[_], A, B] { // F[_] is the context, A is the input, B is the output
  def run: A => F[B]
  def compose[C](instance: Kleisli[F, A, B])(implicit monad: FlatMap[F]): Kleisli[F, A, C] = {
    ???
  }
}

