package monoid

object SetMonoidUnion {
  def combine[A](a: Set[A], b: Set[A]): Set[A] = {
    a union b
  }
  def identity[A](): Set[A] = Set.empty
}

object SetMonoidDiff {
  def combine[A](a: Set[A], b: Set[A]): Set[A] = {
    a diff b
  }
  def identity[A](): Set[A] = Set.empty
}