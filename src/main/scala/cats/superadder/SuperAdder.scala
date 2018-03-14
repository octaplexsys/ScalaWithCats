package cats.superadder

import cats.Monoid

object SuperAdder {
  def add(items: List[Option[Int]]): Int = {

//    This doesn't work :-(
//    items.fold(0){case(o1: Option[Int], o2: Option[Int]) =>
//      Monoid[Int].combine(o1.getOrElse(Monoid[Int].empty), o2.getOrElse(Monoid[Int].empty))
//    }

    import cats.instances.list._
    import cats.instances.option._
    import cats.instances.int._

    Monoid[Option[Int]].combine(Option(23), Option(43))

    items.fold(0)((o1, o2) =>
    o1
    )

    Monoid[List[Option[Int]]].combine(items, Nil)


  }

  /*
   items.fold(0)(_ + _)
   items.sum // Handles List[Int]
   items.flatten.sum // Handles List[Option[Int]]
  */
}
