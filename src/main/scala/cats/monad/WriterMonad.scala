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

  // get value out of a Writer
  println(writer.value)
  // get log out of Writer
  println(writer.written)
  // or get both!
  val (log, value) = writer.run

  val w1 = for {
    v1 <- 534.pure[Logged]
    _ <- Vector("log 1", "log 2").tell
    v2 <- Writer(Vector("log3"), 10)
  } yield v1 + v2

  println(w1.run)

  val w2 = w1.mapWritten(logLines => logLines.map(_.toUpperCase))

  println(w2.run)
}