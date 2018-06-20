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

  def tacticalReport(ally1: String, ally2: String)(implicit ec: ExecutionContext): String = {
    val either = Await.result(canSpecialMove(ally1, ally2).value, Duration.Inf)
    either match {
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge"
      case Left(str) => str
    }
  }


  locally {
    import scala.concurrent.ExecutionContext.Implicits.global

    println(Await.result(getPowerLevel("Jazz").value, Duration.Inf))
    println(Await.result(getPowerLevel("NOTTHERE").value, Duration.Inf))

    val resultOfSpecialMove: Future[Either[String, Boolean]] = canSpecialMove("Jazz", "Hot Rod").value
    println(Await.result(resultOfSpecialMove, Duration.Inf))

    val resultOfTacticalReport1 = tacticalReport("Jazz", "Bumblebee")
    println(resultOfTacticalReport1)

    val resultOfTacticalReport2 = tacticalReport("Jazz", "Hot Rod")
    println(resultOfTacticalReport2)

    val resultOfTacticalReport3 = tacticalReport("Jazz", "Ironhide")
    println(resultOfTacticalReport3)

  }
}
