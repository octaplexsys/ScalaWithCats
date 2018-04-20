package cats.monad

import cats.data.Reader

case class Db(usernames: Map[Int, String],
              passwords: Map[String, String])
class RunDatabaseReader {
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
      bool <- for {
        username <- maybeUsername
      } yield checkPassword(username, password)
    } yield bool
  }
}
