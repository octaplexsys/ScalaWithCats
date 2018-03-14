package cats.superadder

import cats.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.instances.list._
import cats.syntax.all._

object SuperAdder {
  def add(items: List[Option[Int]]): Int = {
    // WHY NOT?
    items.flatten.sum // Handles List[Option[Int]]

    //This works but why would you do this :-(
    items.foldLeft(Monoid[Int].empty)((acc: Int, opt: Option[Int]) => Monoid[Int].combine(acc,opt.getOrElse(Monoid[Int].empty)))

    // Option 1.
    items.combineAll.orEmpty
    Monoid[Option[Int]].combineAll(items).getOrElse(Monoid[Int].empty)

    // Option 2.
    items.flatten.combineAll
  }

  def add[A](items: List[Option[A]])(implicit M: Monoid[A]): A = { // OR add[A : Monoid] without the implicit parameter
    Monoid[Option[A]].combineAll(items).orEmpty
  }
}

case class Order(totalCost: Double, quantity: Double)
// You might also implement combine here on the case class then call it in the companion object which uses cats,
// in this way you can support both ppl who use cats and those who do not.

object Order {
  val empty: Order = Order(0,0) // Because you don't want to instantiate a new order every time you call .empty

  implicit val monoidOrder2: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order.empty

    override def combine(x: Order, y: Order): Order =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }
}