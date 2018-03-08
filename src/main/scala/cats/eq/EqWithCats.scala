package cats.eq

import java.util.Date

import cats.Eq
import cats.instances.int._
import cats.instances.option._
import cats.syntax.eq._
import printable.Cat

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

  implicit val customDateEq: Eq[Date] = {
    Eq.instance[Date]( (date1: Date, date2: Date) =>
      date1.getTime == date2.getTime
    )
  }

  val x = new Date()
  val y = new Date()
  x === y

  implicit val catEq: Eq[Cat] = {
    Eq.instance[Cat]( (cat1 : Cat, cat2: Cat) =>
      cat1.name == cat2.name &&
      cat1.color == cat2.color &&
      cat1.age == cat2.age
    )
  }

  val cat1 = Cat("meow", 593, "blue")
  val cat2 = Cat("badCat", 593, "red")

  cat1 === cat2
}
