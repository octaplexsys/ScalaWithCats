package printable

import printable.PrintableInstances._

import scala.language.higherKinds

case class Cat(name: String, age: Int, color: String)

trait Printable[A] { self =>
  def format(a: A): String

  def contramap[B](f: B => A): Printable[B] = {
    new Printable[B] {
      override def format(b: B): String = self.format(f(b))
    }
  }
  // MAP : F[A], A =>B, F[B]
  // CMap : F[A], B => A, F[B]
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

object TryPrintable extends App {

  case class Person(name: String, address: String, age: Int)
  val printName: Printable[Person] = printableString.contramap((p: Person) => { println("converting a person to a name"); p.name})

  case class Dog(name: String, age: Long, owner: Person)
  val printOwnerName: Printable[Dog] = printName.contramap((dog: Dog) => {println("converting a dog to a person"); dog.owner})

  val rebecca = Person("rebecca", "123 Fake Street", -99)
  val corgi = Dog("Mr. Corgi", 2, rebecca)

  val dogsOwner: String = printOwnerName.format(corgi)

  println(printName.format(rebecca))
  println(dogsOwner)

  val vanityAgePrinter: Printable[Person] = printableInt.contramap((p: Person) => {println("lying on a government form"); p.age - 10})
  println(vanityAgePrinter.format(Person("Matt", "321 Fake Street", 29)))

}

