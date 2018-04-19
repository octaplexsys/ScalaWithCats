package cats.monad
import cats.Id
import cats.data.{Kleisli, Reader}

case class Cat(name: String, age: Int)

object ReaderMonad extends App {
// Reader is used to sequence computations that depend on an input
  def catsName(c: Cat):String = c.name

  // use the apply method from Reader
  val readCatsName: Reader[Cat, String] = Reader(catsName)

  // we are extracting and running the catsname function
  val meowKitty: Id[String] = readCatsName.run(Cat("Meow", 12))

  println(meowKitty)

  // you can map over a Reader
  val greetKitty: Reader[Cat, String] = readCatsName.map(name => s"hello $name")
  // Ask team this
  val whyIsThisAlsoKleisli: Kleisli[Id, Cat, String] = readCatsName.map(name => s"hello $name")

  val tigerCat = Cat("Tiger", 5)
  println(greetKitty.run(tigerCat))

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of fish")

  // flatmap is supported by reader
  val greetAndFeed = {
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet. $feed"
  }

  // Different syntax
  val flatMapReader = greetKitty.flatMap(greet => feedKitty.map(feed => s"$greet. $feed" ))

  println(flatMapReader.run(tigerCat))
  println(greetAndFeed.run(tigerCat))

}
