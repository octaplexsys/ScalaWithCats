import java.util.{Date, UUID}

import json.Person
import printable.{Cat, Printable}

object Main extends App {
  // JSON writer examples
  import json.JsWriterInstances._
  import json.Json._
  println(toJson("Hello"))
  println(toJson(389474))
  println(toJson(Person("name", "emailhere")))
  println(toJson(Option("testOptionString")))
//  println(toJson(Some("stringTest"))) // THIS WILL FAIL
  println(toJson(Some("stringTest"): Option[String]))
  println(toJson(Companion.some("sfjskdfs")))
//  println(toJson(None)) // THIS WILL FAIL
  println(toJson(Companion.none[String]))

  // Common pattern seen in Cats
  object Companion {
      def some[A](a: A): Option[A] = Some(a)
      def none[A]: Option[A] = None
  }

  // Printable example
  import printable.PrintableInstances._
  Printable.print(2348279)
  Printable.print("thisisastring")
  Printable.print(Cat("catName", 349, "blue"))

  // Extension methods
  import printable.PrintableSyntax._
  Cat("ExtensionMethodExample", 573, "purple").print
  "Hello extension method on string".print
  ("can these be combined " + 5).print

  // CatsShow
  import cats.show.ShowWithCatsExtensions._
  import cats.syntax.show._ // Why do I need these here too???

  val date = new Date()
  println(date.show)
  println(UUID.randomUUID().show)
  println(Cat("bob", 384, "red").show)

  //CatsEq
  import cats.eq.EqWithCats._
  import cats.syntax.eq._

  val cat1 = Cat("Garfield",
    38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  println(cat1 === cat2)
  println(cat1 === cat1)

  import cats.instances.option._
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]
  println(optionCat1 === optionCat2)

  // SuperAdder
  import cats.superadder.SuperAdder._
  println(add(List(None,Some(2),Some(3),Some(4))))
  import cats.superadder.Order._
  import cats.instances.int._

  println(add2(List(None,Some(2),Some(3),Some(4))))
  println(add2(List(Option(cats.superadder.Order(1.0, 4.2)), Option(cats.superadder.Order(2.1,5.6)))))
}
