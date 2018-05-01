package monad

trait StateMonad[S,B] {
  def pure(b: B):StateMonad[S, B]
  def flatMap[A](f: A => StateMonad[S, B]): StateMonad[S, B]
}
// HALP
case class StateM[S, A](stateFunction: A => StateMonad[S, A]) extends StateMonad[S, A] {
  override def pure(a: A): StateMonad[S, A] = stateFunction(a)

  override def flatMap[B](f: A => StateMonad[S, B]): StateMonad[S, B] = ???
}


