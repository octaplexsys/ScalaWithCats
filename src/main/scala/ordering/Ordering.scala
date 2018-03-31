package ordering
// Animal
// Doge <: Animal

// Ordering[Doge] -> Ordering[Animal]? n
// Ordering[Animal] -> Ordering[Doge]? y
trait Ordering[-A] { self =>
  def compareStuff(a1: A, a2: A): Int // classes

  def contramap[B](f: B => A): Ordering[B] = {
    new Ordering[B] {
      override def compareStuff(b1: B, b2: B): Int = self.compareStuff(f(b1), f(b2))
    }
  }
}

case class Dollar(dollars: Long, cents: Long)

object Ordering {
  implicit val dollarOrderer = new Ordering[Dollar] {
    override def compareStuff(a1: Dollar, a2: Dollar): Int = {
      (a1.dollars * 100 + a1.cents, a2.dollars * 100 + a2.cents) match {
        case (a1InCents, a2Incents) if a1InCents > a2Incents => 1
        case (a1InCents, a2Incents) if a1InCents == a2Incents => 0
        case _ => -1
      }
    }
  }

  implicit def reverseCompare[A](implicit oa: Ordering[A]): Ordering[Reverse[A]] = {
    new Ordering[Reverse[A]] {
      override def compareStuff(ra1: Reverse[A], ra2: Reverse[A]): Int = {
        val nonReverseCompare = oa.compareStuff(ra1.value, ra2.value)
        if (nonReverseCompare == -1) 1
        else if( nonReverseCompare == 0) 0
        else -1
      }
    }
  }
}

object OrderingInterface {
  def compare[A](thing1: A, thing2: A)(implicit o: Ordering[A]): Int = o.compareStuff(thing1, thing2)

  def contramap[A, B](f: B => A)(implicit o: Ordering[A]): Ordering[B] = o.contramap(f)
}

case class Yen(value: Long)

case class Reverse[A](value: A)

object RunOrdering extends App {

  // if (a <= b) then Reverse(b) <= Reverse(a)

  import Ordering._
  import OrderingInterface._

  val d1 = Dollar(1, 50)
  val d2 = Dollar(40, 18)

  val compareDollars1 = compare(d1, d2)
  val compareDollars2 = compare(d2, d1)
  val compareDollars3 = compare(d2, d2)

  println(compareDollars1)
  println(compareDollars2)
  println(compareDollars3)

  // Goal: contramap Yen to dollars and compare it

  val y1 = Yen(500)
  val y2 = Yen(400)

  def toDollars(y: Yen): Dollar = {
    val stuff = y.value * 0.0094
    val billz = stuff.round
    val penniez = ((stuff - billz) * 100).round
    Dollar(billz, penniez)
  }

  val compareYen: Ordering[Yen] = contramap(toDollars)

  val result = compareYen.compareStuff(y1, y2)
  println(result)

  val result2 = reverseCompare(compareYen).compareStuff(Reverse(y1), Reverse(y2))
  println(result2)
}