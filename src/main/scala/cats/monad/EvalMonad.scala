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
}
