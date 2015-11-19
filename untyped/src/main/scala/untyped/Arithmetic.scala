package untyped

object Arithmetic {
  sealed trait Expression {
    def eval: Double =
      this match {
        case Plus(l, r)     => l.eval + r.eval
        case Minus(l, r)    => l.eval - r.eval
        case Multiply(l, r) => l.eval * r.eval
        case Divide(l, r)   => l.eval / r.eval
        case Value(v)       => v
      }

    def +(that: Expression): Expression =
      Plus(this, that)
    def -(that: Expression): Expression =
      Minus(this, that)
    def *(that: Expression): Expression =
      Multiply(this, that)
    def /(that: Expression): Expression =
      Divide(this, that)
  }
  object Expression {
    // Smart constructor that creates Value with type Expression
    def value(in: Double): Expression =
      Value(in)
  }
  final case class Plus(left: Expression, right: Expression) extends Expression
  final case class Minus(left: Expression, right: Expression) extends Expression
  final case class Multiply(left: Expression, right: Expression) extends Expression
  final case class Divide(left: Expression, right: Expression) extends Expression
  final case class Value(get: Double) extends Expression

}
