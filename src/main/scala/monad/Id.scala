package monad

import cats.Monad

object Id {
  type Id[A] = A
  val monadId: Monad[Id] = new Monad[Id] {
    override def pure[A](a: A): Id[A] = a

    override def flatMap[A, B](value: Id[A])(f: A => Id[B]): Id[B] = f(value)

    override def map[A, B](value: Id[A])(f: A => B): Id[B] = f(value)
  }
}