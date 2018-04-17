package cats.monad
import cats.Id
import cats.data.{Writer, WriterT}
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

// Writer monad is used for keeping track of things as you're computing things.
// focus on data that matters while collecting something else
object WriterMonad extends App {
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
  def logLine(logLines: Vector[String]): Vector[String] = {logLines.map(_ + "!")}
  def number(i: Int) = i + 2
  // you can use bimap on a writerMonad
  val result1 = w2.bimap(logLine, number)
  println(result1)

  // you can use mapBoth on a writerMonad too
  val result2 = w2.mapBoth((strings, int) => (strings.map(_ + "!!"), int + 1 ))
  println(result2)

  // you can swap the l and right side of a writer monad
  println(w1.swap)

  // you can reset the writer
  println(w1.reset)
  println(w1)
}