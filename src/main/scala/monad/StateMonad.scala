package monad
trait StateMonad[S,A] {
  def map[B](f: A => B): StateMonad[S, B]
  def flatMap[B](f: A => StateMonad[S, B]): StateMonad[S, B]
  def run(state: S): (S, A)
  def runA(state: S): A =
    run(state)._2
  def runS(state: S): S =
    run(state)._1
}
// get == StateM(s => (s, s))
// set(30) == StateM(_ => (30, ())
// pure(30) == StateM(s => (s, 30))
// inspect(f: S => B) == StateM(s => (s, f(s)))
// modify(f: S => A) == StateM(s => (f(s), ())
object StateMonad {
  // Needs a new name now.
  case class MappedModify[S, A](func: S => (S, A)) extends StateMonad[S, A] {
    override def run(state: S): (S, A) =
      func(state)
    // modify : s => s
    // inspect : s => a
    override def map[B](f: A => B): StateMonad[S, B] =
      MappedModify {s =>
        val (s1, a) = func(s)
        s1 -> f(a)
      }
    override def flatMap[B](f: A => StateMonad[S, B]): StateMonad[S, B] = {
      MappedModify{ s: S =>
        // 1. inside a MappedModify, given an initial state, run it to get a secondary state and a value
        val(s1, a) = func(s)
        // 2. apply the function which produces another StateMonad to the resulting value
        val stateMonad2 = f(a)
        // 3. the new stateMonad will have a run method, which takes in our secondary state and returns a new state and value
        stateMonad2.run(s1)
      }
    }
  }

  def get[S]: StateMonad[S, S] = MappedModify(s => (s, s))
  //  def set[S](state: S): StateMonad[S, Unit] = MappedModify(_ => state, _ => ())
  def set[S](state: S): StateMonad[S, Unit] = MappedModify(_ => (state, ()))
  //  def pure[S, A](result: A): StateMonad[S, A] = MappedModify(identity, _ => result)
  def pure[S, A](result: A): StateMonad[S, A] = MappedModify(s => (s, result))
  //  def inspect[S, A](f: S => A): StateMonad[S, A] = MappedModify(identity, f)
  def inspect[S, A](f: S => A): StateMonad[S, A] = MappedModify(s => (s, f(s)))
  //  def modify[S](f: S => S): StateMonad[S, Unit] = MappedModify(f, _ => ())
  def modify[S](f: S => S): StateMonad[S, Unit] = MappedModify(s => (f(s), ()))

  def traverse[S,A,B](xs: Seq[A])(f: A => StateMonad[S, B]): StateMonad[S, Seq[B]] = {
    xs.foldLeft[StateMonad[S, Seq[B]]](pure(Seq())){case(acc, next) => acc.flatMap(seqB => f(next).map(seqB :+ _))}
  }
}

object Test extends App {
  import StateMonad._
  val program: StateMonad[Int, (Int, Int, Int)] =
    for {
      a <- get[Int]
      _ = println(a.toString)
      _ <- set[Int](a + 1)
      b <- get[Int]
      _ = println(b)
      _ <- modify[Int](_ + 1)
      c <- inspect[Int, Int](_ * 1000)
      _ = println(c)
    } yield (a, b, c)
  program.run(1)
}

