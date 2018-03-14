import scala.collection.mutable.ArrayBuffer

object CString {

  val sb: StringBuilder = new StringBuilder(10) // MONOMORPHIZED / Monomorphic
  val javasb: StringBuffer = ???
  val ab: ArrayBuffer[Char] = new ArrayBuffer[Char]()

  val NUL = '\0'

  Array[Char]('h','e','l','l','o',NUL)
  type CString = ArrayBuffer[Char]
}
