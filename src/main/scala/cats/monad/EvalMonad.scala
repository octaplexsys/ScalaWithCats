package cats.monad

import cats.Eval
import cats.instances.double._

object EvalMonad extends App {
  import cats.syntax.show._

  val now: Eval[Double] = Eval.now(math.random()) // eager, memoized
  val later: Eval[Double] = Eval.later(math.random()) // always captures evaluation, lazily, like a def
  val always: Eval[Double] = Eval.always(math.random()) // lazy memozied, like lazy val

  println(now)
  println(later)
  println(always)

  println(now.value.show)
  println(later.value.show)
  println(always.value.show)

  val evalTest: Eval[String] = Eval.later("hello later")
    .map{s =>
      println("step1")
      s + " and then map"}
    .flatMap{ s =>
      println("step2")
      Eval.now(s + " finally flatmap")}
  println(evalTest)
  println(evalTest.value) // once
  println(evalTest.value) // twice
  // Note, that Eval.value is evaluated as a def would be.

  // you can memoize the results of something partway through a chain of things to evaluate
  val evalWithMemoize: Eval[Int] = Eval.now(0).map {
    (i) =>
      println("first step")
      i + 1
  }.flatMap { (j) =>
    println("second step")
   Eval.always(j + 1)
  }.memoize
    .map {(h) =>
      println("third step")
      h + 1
  }

  println
  println(evalWithMemoize.value)
  println(evalWithMemoize.value)
  // This call to get the memoized value of Eval doesn't re-evaluate the memoized portion of the chain

}
