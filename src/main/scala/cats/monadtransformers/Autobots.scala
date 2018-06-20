package cats.monadtransformers

import cats.data.EitherT
import cats.implicits._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object Autobots extends App {
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee"-> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String)(implicit ec: ExecutionContext): Response[Int] = {
    EitherT.fromOption[Future](powerLevels.get(autobot), s"Autobot: $autobot not found")
  }

  def canSpecialMove(ally1: String, ally2: String)(implicit ec: ExecutionContext): Response[Boolean] = {
    val canDo: EitherT[Future, String, Boolean] = for {
     power1 <- getPowerLevel(ally1)
     power2 <-getPowerLevel(ally2)
    } yield (power1 + power2) >= 15

    canDo.leftMap[String](_ => "Not able to do special move")
  }


  locally {
    import scala.concurrent.ExecutionContext.Implicits.global

    val result: Future[Either[String, Int]] = getPowerLevel("Jazz").value
    println(Await.result(result, Duration.Inf))
    println(Await.result(getPowerLevel("NOTTHERE").value, Duration.Inf))

    val resultOfSpecialMove: Future[Either[String, Boolean]] = canSpecialMove("Jazz", "Hot Rod").value
    println(Await.result(resultOfSpecialMove, Duration.Inf))

  }
}
