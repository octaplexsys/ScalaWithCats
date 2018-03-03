import json.Person
import printable.{Cat, Printable}

object Main extends App {
  import json.JsWriterInstances._
  import json.Json._

  println(toJson("Hello"))
  println(toJson(389474))
  println(toJson(Person("name", "emailhere")))
  println(toJson(Option("testOptionString")))
  //println(toJson(Some("stringTest"))) THIS WILL FAIL
  println(toJson(Some("stringTest"): Option[String]))
  println(toJson(Companion.some("sfjskdfs")))
  //println(toJson(None)) THIS WILL FAIL
  println(toJson(Companion.none[String]))


    // Common pattern seen in Cats
    object Companion {
        def some[A](a: A): Option[A] = Some(a)
        def none[A]: Option[A] = None
    }


  import printable.PrintableInstances._
  Printable.print(2348279)
  Printable.print("thisisastring")
  Printable.print(Cat("catName", 349, "blue"))
}
