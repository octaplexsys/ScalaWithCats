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

  def factorial(n: BigInt): BigInt = {
    if (n == 1) n
    else {
      n * factorial(n - 1)
    }
  }
  println("factorial")
  println(factorial(1))
//  println(factorial(50000)) nope
  def factorialStackSafe(n: BigInt): Eval[BigInt] = {
    if(n == 1) Eval.now(n)
    else {
      Eval.defer(factorialStackSafe(n - 1).map(_ * n))
      // we need the Eval.defer to trampoline our recursive call to factorial
    }
  }
  println("factorialStackSafe")
//  println(factorialStackSafe(50000).value) // this does not stack overflow but it is slow

  def foldRightNaive[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = {
    as match {
      case Nil => acc
      case h :: t => fn(h, foldRightNaive(t, acc)(fn))
    }
  }

  /// println(foldRightNaive(1 to 100000 toList, 1)((next: Int, acc: Int) => next * acc)) // not stack safe
  println("foldR")
  println(foldRightNaive(1 to 10 toList, 1)((next: Int, acc: Int) => next * acc)) // not stack safe

  def foldRight[A, B](as: List[A], acc: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
    as match {
      case Nil => acc
      case h :: t => Eval.defer(f(h, foldRight(t, acc)(f)))
    }
  }

  private val foldRightWithEval: Eval[Int] = foldRight(1 to 100000 toList, Eval.now(1))((next: Int, acc: Eval[Int]) => acc.map(_ * next))
  println("FoldRightEval")
  println(foldRightWithEval.value)


}
