package cats.monad

import cats.MonadError
import cats.instances.either._

class TestErrors {
  type ErrorOr[A] = Either[String, A] // <-- this is actually needed to make an Either have only one "bucket"

  // ({type L[A] = Either[String, A]})#L <== Kind projector. Anonymous type param.
  def monadError = MonadError[ErrorOr, String]
  val good: Either[String, Long] = monadError.pure(47893L)
  val bad: ErrorOr[Nothing] = monadError.raiseError("It failed")
  // handleError is like calling recover on a future.
  val handlingError: ErrorOr[ErrorOr[Long]] = monadError.handleError(bad) {
    case "Recoverable" => monadError.pure(123L)
    case "It failed" => monadError.raiseError("Definitely failed")
    case _ => monadError.raiseError("Who knows")
  }
  // ensure is like a filter
  // you call ensure on a MonadError instance
  // it's first argument is the monad in question, the second is the fallback, the third is the predicate
  monadError.ensure(good)("Did not work")((i: Long) => i < 10)
}