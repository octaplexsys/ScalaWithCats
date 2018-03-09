package cats.eq


trait EqPractice[A] {

}
trait Ord[A] extends EqPractice[A] {
  def compare(x: A, y:A ): Int

  def eqv(x: A, y: A) = {
    compare(x,y) == 0
  }
}