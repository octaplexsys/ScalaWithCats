package cats.semigroupal
import cats.Semigroupal
import cats.instances.option._


case class Cat(name: String, age: Int, color: String)

object TestSemigroupal extends App {
  val sgOpt1 = Semigroupal[Option].product(Some(213), Some("ABC"))
  println(sgOpt1)

  val sgOpt2 = Semigroupal[Option].product(Some(355), None)
  println(sgOpt2)

  val sgTuple3 = Semigroupal.tuple3(Option(1), Option(2), Option(5))
  println(sgTuple3)

  val sgMap3 = Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
  println(sgMap3)

  import cats.syntax.apply._

  val sgTupledApply = (Option(123), Option("SDJFLskfd"), Option("sjdfkl")).tupled
  println(sgTupledApply)

  val optionalcat = (Option("Garfield"), Option(1213), Option("Orange")).mapN{Cat}
  println(optionalcat)



}
