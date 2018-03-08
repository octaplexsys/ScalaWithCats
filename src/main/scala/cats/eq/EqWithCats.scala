package cats.eq

import cats.Eq
import cats.instances.int._
import cats.instances.option._
import cats.syntax.eq._

object EqWithCats {

  val eqInt = Eq[Int]

  eqInt.eqv(0, 0)

  // eqInt.eqv("434", 434) // Doesn't compile

  "3434" == 3242 // Weirdly compiles

  1 === 2 // We imported the cats syntax for eq so we can use this
  1 =!= 5

  (Some(2): Option[Int]) === (None: Option[Int]) // Parens are very important here
  Option(1) === Option.empty[Int] // This works

  import cats.syntax.option._
  none[Int] =!= 45.some


  import cats.instances.string._
  "hello".some =!= "blah".some
}
