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

  println(greetKitty.run(Cat("Tiger", 4)))

}
