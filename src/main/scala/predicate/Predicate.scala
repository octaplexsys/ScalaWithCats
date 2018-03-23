package predicate

/* Is Predicate contravariant? Yes.

Dog <: Animal

If I have a predicate of dog, do I have a predicate of animal? Y
If I have a predicate of animal, do I have a predicate of dog? N

If you have a predicate is? Dog, you have a predicate is? Animal
But if you have a predicate is? Animal, you can't use it to determine if something is a dog.

So Predicate[Dog] is the superclass of Predicate[Animal]
*/

trait Predicate[-A] { self =>

  def apply(a: A): Boolean

  /* Explanation using Apples:
  * Say I have a predicate that operates on Apples. It says whether the apple is red.
  * Say I have a candy apple, I'm going to pre-process it so that I can tell whether
  * it is red using a function f which eats the candy wrapper (thereby turning it into an Apple).
  * Because I have this function, I have a Predicate that works on candy apples!
  * */

  def contramap[B](f: B => A): Predicate[B] = {
    this apply f(_)
  }
}

object PredicateInstances {
  implicit val isRed = new Predicate[Apple] {
    override def apply(a: Apple): Boolean = a.color.toLowerCase == "red" // Does this mean I only get one predicate for all of apples???
  }
}

case class Apple(color: String)
case class CandyWrapper(apple: Apple)

object TryPredicate extends App {
  import PredicateInstances._

  val candiedAppleGreen = CandyWrapper(Apple("Green"))
  val candiedAppleRed = CandyWrapper(Apple("red"))

  val inspectWrappedApple: Predicate[CandyWrapper] = isRed.contramap((candyWrapper: CandyWrapper) => candyWrapper.apple)

  println(inspectWrappedApple(candiedAppleGreen))
  println(inspectWrappedApple(candiedAppleRed))

}