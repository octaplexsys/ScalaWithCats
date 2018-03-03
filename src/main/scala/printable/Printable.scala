package printable

trait Printable[A] {
def format(a: A): String
}
object PrintableInstances{
  implicit val printableString = {
    new Printable[String] {
       def format(a: String): String = a
    }
  }

  implicit val printableInt = {
    new Printable[Int] {
      def format(a: Int) = a.toString
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