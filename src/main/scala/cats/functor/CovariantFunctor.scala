package cats.functor

import scala.language.higherKinds

//trait CovariantFunctor[CoF[_]] {
trait CovariantFunctor[F[_]] {
  // If you tell me how to turn A into B, I will turn my A into a B
  // Changing things that I store or produce
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

//trait ContravariantFunctor[ContraF[_]] {
trait ContravariantFunctor[F[_]] {
  // If you me how to turn B into A, I will use that to turn Bs into As, as I already know how to use As
  // Changing things that I consume
  def contramap[A,B](fa: F[A])(f: B => A): F[B]
}

trait Profunctor[F[_,_]] {
  // I can turn Cs into As, convert the As into Bs, then change the Bs into Ds
  // I consume As, and produce Bs, and I can change both
  def bimap[A,B,C,D](pab: F[A,B])(f: C => A, g: B => D): F[C,D]
}