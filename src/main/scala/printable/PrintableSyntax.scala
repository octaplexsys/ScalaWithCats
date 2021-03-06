package printable

// EXTENSION METHODS
object PrintableSyntax {
  implicit class PrintableOps[A](a: A) {

    def format(implicit printableA: Printable[A]): String = {
      printableA.format(a)
    }

    def print(implicit printableA: Printable[A]): Unit = {
      println(printableA.format(a))
    }
  }
}
