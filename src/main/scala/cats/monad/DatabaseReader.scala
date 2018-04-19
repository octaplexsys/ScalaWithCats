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
}
