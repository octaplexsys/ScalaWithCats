sealed trait Maths[A]
case class Constant(v: Int) extends Maths[Int]
case class Plus[A](x: Maths[A], y: Maths[A]) extends Maths[A]

Plus(Constant(1), Plus(Constant(2), Constant(3)))

def eval(maths: Maths[Int]): Int = maths match {
  case Constant(v) => v
  case Plus(x, y) => eval(x) + eval(y)
}