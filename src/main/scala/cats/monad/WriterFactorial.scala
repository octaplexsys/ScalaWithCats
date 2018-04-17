package cats.monad

import cats.data.Writer
import cats.syntax.applicative._
import cats.instances.vector._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


object WriterFactorial extends App {

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  type Logged[A] = Writer[Vector[String], A]

  def factorialW(n: Int): Logged[Int] = {
    val runFact = {if (n == 0) 1.pure[Logged] // pure comes from applictative
    else factorialW(n - 1).bimap(loglines => loglines :+ s"fact $n", i => i * n )}
    slowly(runFact)
  }

  Await.result(Future.sequence(Vector(
    Future(factorial(3)),
    Future(factorial(3))
  )), 5.seconds)

  println(factorialW(4).run)

}
