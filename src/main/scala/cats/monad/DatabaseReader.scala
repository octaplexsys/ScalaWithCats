package cats.monad

import cats.data.{Kleisli, Reader}
import cats.syntax.applicative._

case class Db(usernames: Map[Int, String],
              passwords: Map[String, String])
object RunDatabaseReader extends App {
  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))
  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    Reader{db =>
      val maybeUsername = db.usernames.get(userId)
      val maybePassword = maybeUsername.flatMap(username => db.passwords.get(username))
      maybePassword.fold(false)(foundPassword => foundPassword == password)
    }

  def checkLogin2(userId: Int, password: String): DbReader[Boolean] = {
    for {
      maybeUsername <- findUsername(userId)
      bool <- {
        val maybeReader: Option[DbReader[Boolean]] = for {
          user <- maybeUsername
        } yield checkPassword(user, password)
        maybeReader.getOrElse(Kleisli.pure(false))
      }
    } yield bool
  }

  def checkLogin3(userId: Int, password: String): DbReader[Boolean] = {
    for {
      maybeUsername <- findUsername(userId)
      bool <- {
        val maybeReader: Option[DbReader[Boolean]] = for {
          user <- maybeUsername
        } yield checkPassword(user, password)
        maybeReader.getOrElse(Kleisli.pure(false))
      }
    } yield bool
  }

  def checkLogin4(userId: Int, password: String) = for {
    username   <- findUsername(userId)
    passwordOk <- username.map { username =>
      checkPassword(username, password)
    }.getOrElse {
      false.pure[DbReader]}
    }yield passwordOk

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )
  val passwords = Map(
    "dade"  -> "zerocool",
    "kate"  -> "acidburn",
    "margo" -> "secret")

  val db = Db(users, passwords)
  println(checkLogin(1, "zerocool").run(db))
  // res10: cats.Id[Boolean] = true
  println(checkLogin(4, "davinci").run(db))
  // res11: cats.Id[Boolean] = false

  println(checkLogin2(1, "zerocool").run(db))
  // res10: cats.Id[Boolean] = true
  println(checkLogin2(4, "davinci").run(db))
  // res11: cats.Id[Boolean] = false
}