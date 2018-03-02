sealed trait Json
case object JsNull extends Json
case class JsString(get: String) extends Json
case class JsNumber(get: Int) extends Json
case class JsObject(get: Map[String, Json]) extends Json

case class Person(name: String, email: String)

trait JsWriter[A] { // General rule: typeclass is always invariant
  def write(a: A): Json
}

object JsWriterInstances {
  implicit val stringWriter = {
    new JsWriter[String] {
      def write(a: String) = JsString(a)
    }
  }

  implicit val numberWriter = {
    new JsWriter[Int] {
      def write(a: Int) = JsNumber(a)
    }
  }

  implicit val objectWriter = {
    new JsWriter[Person] {
      def write(a: Person) = JsObject(
        Map(
          "name" -> JsString(a.name),
          "email" -> JsString(a.email)
        )
      )
    }
  }

  implicit def optionWriter[A](implicit jsWriter: JsWriter[A]): JsWriter[Option[A]] = {
    new JsWriter[Option[A]] {
      def write(a: Option[A]): Json = {
        a match {
          case None => JsNull // You don't need an implicit writer
          case Some(aThing) => jsWriter.write(aThing)
        }
      }
    }
  }
}

object Json {
  def toJson[A](a: A)(implicit jsWriter: JsWriter[A]): Json = jsWriter.write(a)
}