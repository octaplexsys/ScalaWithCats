package cats.monad

import cats.data.State

object RunStateMonad extends App {
  val s: State[Int, String] = State{(stateInt: Int) => (stateInt + 1, s"The state is $stateInt") }

  // Note: Run returns an Eval for stack safety, so we need to call value to extract it.
  val (state, string) = s.run(10).value
  val stateWithoutValue = s.runA(10).value
  val valueWithoutState = s.runS(10).value

  println(stateWithoutValue)
  println(valueWithoutState)

  val step1 = State{(stateString: String) => (stateString + "!", 1)}
  val step2 = step1.map(i => i + 1)
  println(step2.run("statefulString").value)

}
