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

  // get extracts the state as the result
  val getState: State[Int, Int] = State.get[Int] // which is why both L and R sides of state are same
  // Sets the L parameter, the state. The R value is Unit.
  val setState: State[Int, Unit] = State.set[Int](40)
  // Pure ignores the state and returns the result
  val pureState: State[Int, String] = State.pure("HI")
  // Inspect extracts the state via a transformation function
  val inspectState = State.inspect[Int, String](_ + "STRINGCHANGE")
  println(inspectState.run(50).value)
  // Modify updates the state using an update function
  val modifyState = State.modify[Int]( _ + 49)
  println(modifyState.run(5).value)

}
