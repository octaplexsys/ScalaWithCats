package codec

trait Codec[A] { self =>
  def encode(value: A): String
  def decode(value: String): A

  // We start with a codec of A, so the goal is to transform Codec[A] into Codec[B]
  // imap takes 2 functions, one which takes an A => B and another that takes a B => A and we need a codec of B at the end
  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
    new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))
      override def decode(string: String): B = dec(self.decode(string))
    }
  }
}

object CodecInstances {
  implicit val stringCodec: Codec[String] = new Codec[String] {
    override def encode(value: String): String = value

    override def decode(value: String): String = value
  }

  implicit val intCodec: Codec[Int] = new Codec[Int] {
    override def encode(value: Int): String = value.toString

    override def decode(value: String): Int = value.toInt
  }

  implicit val doubleCodec: Codec[Double] = new Codec[Double] {
    override def encode(value: Double): String = value.toString

    override def decode(value: String): Double = value.toDouble
  }

  implicit def boxCodec[A](implicit codecA: Codec[A]): Codec[Box[A]] = {
    // can I use the imap method from codecA to do this?
    codecA.imap(innerUnboxedA => Box(innerUnboxedA), boxedValue => boxedValue.value)
  }
}

object Codec {
  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)
  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)
}

case class Box[A](value: A)

object TryCodec extends App {
  import Codec._
  import CodecInstances._

  println(encode(21)(intCodec))
  println(encode(21.00)(doubleCodec))

  println(encode(21)(intCodec.imap(s => s , _ * 100)))

  println(intCodec.imap[Int](identity, _ * 1000).encode(21))

  println(encode(Box(59)))
  println(decode[Int]("29"))
  println(decode[Box[Int]]("478"))
}
