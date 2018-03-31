package cats.monad

import scala.util.Try

object CatsEither extends App {
  import cats.syntax.either._
  val e1: Either[Nothing, Int] = Right(1)
  val e2 = Right(3)
  val result = for {
    a <- e1
    b <- e2
  } yield a + b
  println(result)

  val a: Either[String, Int] = 3.asRight[String] // Nice syntax as it returns either instead of l/r
  val b = 6.asRight[String]
  for {
    x <- a
    y <- b
  } yield x + y

  def countPositives(nums: List[Int]): Either[String,Int] = {
    nums.foldLeft(0.asRight[String])((acc, next) =>
      if (next >=0) acc.map(_ + 1 )
      else Left("Negative number fount")
    )
  }

  countPositives(List(1,2,3))
  countPositives(List(1,-2,3))

  // Exception handling
  println(Either.catchOnly[NumberFormatException]("foo".toInt))
  println(Either.catchNonFatal(sys.error("oops")))
  //  println(Either.catchNonFatal(throw new ThreadDeath())) // If you put this back nothing else will run afterwards

  // Either[Left, Right]
  // asLeft and asRight take a type parameter defining the OPPOSITE side of either
  142.asRight[String]: Either[String, Int]
  println("Error".asLeft[Int].getOrElse(0))
  println("error".asLeft[List[String]].orElse(List("Hello").asRight[Int]))
  "Another error".asLeft[Int].orElse(Right(1))
  "Another error".asLeft[Int].orElse(2.asRight[String])

  // Ensure!!!
  println((-1).asRight[String].ensure("mustBeNonNegative")(_ > 0))

  // Making an Either from other types
  Either.fromTry(Try("Hello")): Either[Throwable, String]
  // The first argument to fromOption is the option under focus,
  // the second parameter is what to return if the option is none
  Either.fromOption[String, Int](None, "test")
  Either.fromOption[Long, String](Some("Hi"), 0L)
  Either.fromOption[Long, Nothing](None, 0L) // Does this compile? haha!

}