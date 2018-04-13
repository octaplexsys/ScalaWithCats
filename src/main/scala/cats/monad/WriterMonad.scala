package cats.monad
import cats.Id
import cats.data.{Writer, WriterT}
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._
class WriterMonad {
  // Writer monad allows us to carry log along with computation
  val writer: WriterT[Id, Vector[String], Int] = Writer(Vector("step1", "step2", "step3"), 50475)
  // WriterT is the monad transformer version of Writer

  type Logged[A] = Writer[Vector[String], A]

  val pureWithEmptyLog: Logged[Int] = 123.pure[Logged]

  val logWithNoValue: Writer[Vector[String], Unit] = Vector("Hello").tell

}