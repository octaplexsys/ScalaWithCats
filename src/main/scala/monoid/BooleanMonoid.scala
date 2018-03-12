package monoid

class BooleanMonoidOr {
  def combine(a: Boolean, b: Boolean): Boolean = a || b
  def empty: Boolean = false
}
class BooleanMonoidAnd {
  def combine(a: Boolean, b: Boolean): Boolean = a && b
  def empty: Boolean = true
}
