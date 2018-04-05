package cats.monadtransformers
import cats.MonadError
import cats.instances.either._


class MonadTransformerTest {
  type ErrorOr[A] = Either[String, A] // <-- this is actually needed to make an Either have only one "bucket"

  // ({type L[A] = Either[String, A]})#L <== Kind projector. Anonymous type param.
  def monadError = MonadError[ErrorOr, String]
  val good: Either[String, Long] = monadError.pure(47893L)
  val bad = monadError.raiseError("It failed")
  val handlingError: ErrorOr[ErrorOr[Long]] = monadError.handleError(bad) {
    case "Recoverable" => monadError.pure(123L)
    case "It failed" => monadError.raiseError("Definitely failed")
    case _ => monadError.raiseError("Who knows")
  }
}


