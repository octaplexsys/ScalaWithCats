package eitherer

sealed trait Eitherer[+A, +B] {
  // We need to explicitly say that AA is covariant. by saying that AA is a supertype of A
  // If onFailure was just A, it is in a CONTRAVARIANT position by dint of being a function argument
  def ensure[AA >: A](onFailure: AA)(f: B => Boolean): Eitherer[AA, B] = this match {
    case Lefter(_)  => this
    case Righter(b) => if (f(b)) this else Lefter(onFailure)
  }
}
case class Lefter[A](left: A) extends Eitherer[A, Nothing]
case class Righter[B](right: B) extends Eitherer[Nothing, B]
