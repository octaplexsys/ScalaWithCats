package monad

trait Id[_] { // I feel like this one doesn't / shouldn't need a container type F[_], but does it need a type param???
  def pure[A](a: A): Id[A]
  def unapply[A]: A
  def flatMap[A, B](instance: Id[A])(f: A => Id[B]): Id[B]
  def map[A, B](instance: Id[A])(f: A => B ): Id[B] = flatMap(instance)((a: A) => pure(f(a)))
}

case class Id[A](a: A) extends Id[A] {
  override def pure[A](a: A): Id[A] = this.map(this)(identity)

  override def unapply[A] = a

  override def flatMap[A, B](instance: Id[A])(f: A => Id[B]): Id[B] =
}

