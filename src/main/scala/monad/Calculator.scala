package monad

import scala.util.Try

object Calculator {
  // A is the top of the stack, the List of Int is stack
  type CalcState[A] = StateMonad[List[Int], A]

  def evalAll(sym: String): CalcState[Unit] = {
    val tokens = sym.split(",")
    val test = StateMonad.traverse[List[Int], String, Unit](tokens) { token =>
      val calcSymbol = CalculatorSymbols.apply(token)
      calcSymbol match {
        case Number(i) => StateMonad.modify((initialList: List[Int]) => i :: initialList)
        case operator:Operator => StateMonad.modify((initialState: List[Int]) => {
          val i1 :: i2 :: tail = initialState
          operator.op(i1, i2) :: tail
        })
      }
    }

    test.map(_ => ())
  }

  // This will be sad if tokens was not created from a well formed postfix String
  def recursiveCalculator(tokens: Seq[String], stack: List[Int]): Int = {
    tokens match {
      case Nil => stack.head
      case h :: t =>
        CalculatorSymbols(h) match {
          case Number(value) => recursiveCalculator(t, value :: stack)
          case r: Operator =>
            val one :: two :: rest = stack
            recursiveCalculator(t, r.op(one, two) :: rest)
        }
    }
  }

}

object RunCalc extends App {
  import Calculator._
  val program = for {
    _ <- evalAll("1,2,+")
    _ <- evalAll("3,4,+")
    ans <- evalAll("*")
  } yield ans

  val result = program.run(List())
  println(result)
}


/**VALID THINGS YOU CAN DO ON A CALCULATOR**/
sealed trait CalculatorSymbols

object CalculatorSymbols {
  def apply(symbol: String): CalculatorSymbols = {
    (Operator.parse(symbol) orElse Try(Number(symbol.toInt)).toOption).get // sadness
  }
}

/**OPERATIONS**/
sealed trait Operator extends CalculatorSymbols {
  val representation: String
  def op(int1: Int, int2: Int): Int
}

object Operator {
  val values: List[Operator] = List(Plus, Multiply, Subtract, IntDivide)
  def parse(symbol: String) = values.find(operator => operator.representation == symbol)
}

case object Plus extends Operator {
  val representation = "+"
  def op(int1: Int, int2: Int) = int1 + int2
}

case object Multiply extends Operator {
  val representation = "*"
  def op(int1: Int, int2: Int) = int1 * int2
}

case object Subtract extends Operator {
  val representation: String = "-"
  def op(int1: Int, int2: Int) = int1 - int2
}

case object IntDivide extends Operator {
  val representation: String = "/"
  def op(int1: Int, int2: Int) = int1 / int2

}

/**WHAT IS THIS? A SCHOOL FOR FOR INTS?**/
case class Number(value: Int) extends CalculatorSymbols
