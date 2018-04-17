package cats.monad

import cats.data.Writer
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

  def factorial2(n: Int): Logged[Int] = {
    val evalFact = {
      if (n == 0) Writer(Vector(s"fact $n 1"), 1)
      else factorial2(n - 1) flatMap { r =>
          val current = r * n
          Writer(Vector(s"fact $n $current"), current)
      }
    }
    slowly(evalFact)
  }

  Await.result(Future.sequence(Vector(
    Future(factorial(3)),
    Future(factorial(3))
  )), 5.seconds)

  println(factorial2(3).run)

  Await.result(Future.sequence(Vector(
    Future(println(factorial2(4))),
    Future(println(factorial2(4)))
  )), 5.seconds)

}
