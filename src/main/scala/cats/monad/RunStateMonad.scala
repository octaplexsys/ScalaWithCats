package cats.monad

import cats.Eval
import cats.data.{IndexedStateT, State}

object RunStateMonad extends App {
  val s: State[Int, String] = State {(stateInt: Int) =>
    val result = stateInt + 1
    (result, s"The state is $result")
  }

  // Note: Run returns an Eval for stack safety, so we need to call value to extract it.
  val (resultState, string) = s.run(10).value
  val stateWithoutValue = s.runA(10).value
  val valueWithoutState = s.runS(10).value

  println(stateWithoutValue)
  println(valueWithoutState)

  val stateString = State{ (stateString: String) => (stateString + "!", 1)}
  val stateString2 = stateString.map(i => i + 1)
  println(stateString2.run("statefulString").value)

  val state1 = State[Int, String] {(int: Int) =>
    val ans = int + 1
    (ans, s"result of step1: $ans")
  }
  val state2 = State[Int, String] {(int: Int) =>
    val ans = int * 2
    (ans, s"result of step2: $ans")
  }

  val both: IndexedStateT[Eval, Int, Int, (String, String)] = for {
    one <- state1
    two <- state2
  } yield (one, two) // Hmmm what is this doing? Not a tuple the way I would have expected

  val (state, result) = both.run(42).value
  println(s"state: $state, result: $result")

}
