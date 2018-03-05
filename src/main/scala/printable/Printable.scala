package printable

case class Cat(name: String, age: Int, color: String)

trait Printable[A] {
  def format(a: A): String
}

object PrintableInstances{
  implicit val printableString: Printable[String] = {
    new Printable[String] {
       def format(a: String): String = a
    }
  }

// NOTE: in Scala 2.12 you can do this!
//  implicit val printableString: Printable[String] =
//    (a: String) => a

  implicit val printableInt: Printable[Int] = {
    new Printable[Int] {
      def format(a: Int) = a.toString
    }
  }

// NOTE: this also works.
//  implicit val catPrinter: Printable[Cat] =
//    (a: Cat) => s"${a.name} is a ${a.age} year-old ${a.color} cat."

  implicit def catPrinter(implicit printerInt: Printable[Int], printerString: Printable[String]): Printable[Cat] = {
    new Printable[Cat] {
      def format(a: Cat): String = {
        s"${printerString.format(a.name)} is a ${printerInt.format(a.age)} year-old ${printerString.format(a.color)} cat."
      }
    }
  }

}

object Printable {
  def format[A](a: A)(implicit printableInstance: Printable[A]): String = {
    printableInstance.format(a)
  }

  def print[A](a: A)(implicit printableInstance: Printable[A]): Unit = {
    println(format(a))
  }
}

