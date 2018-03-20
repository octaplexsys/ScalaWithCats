package cats.functor

import cats.Functor
import cats.instances.list._
import cats.instances.option._

object FunctorsAreFun extends App {
  val list1 = List(1,2,3)
  // Notice how neither List nor Option need to be parameterized below.
  // It's because of higher kinded types.
  val list2 = Functor[List].map(list1)(_ * 2)
  println(list2)
  val option1 = Option(123)
  val option2 = Functor[Option].map(option1)(_.toString)
  println(option2)
  val func = (x: Int) => x + 1
  // func is an Int => Int
  val liftedFunc = Functor[Option].lift(func)
  // liftedFunc is an Option[Int] => Option[Int]
  val optionInt: Option[Int] = liftedFunc(option1)
  println(optionInt)
}
