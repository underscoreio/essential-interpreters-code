package untyped

import cats.syntax.apply._

object ArithmeticAndStrings {
  type Result = Xor[String,Value]

  sealed trait Value
  final case class Number(get: Double) extends Value
  final case class Chars(get: String) extends Value

  sealed trait Expression {
    import Expression._

    def eval: Result =
      this match {
        case Plus(l, r)     =>
          (l.eval |@| r.eval){ applyBinaryNumericOp(_ + _) _ }
        case Multiply(l, r) =>
          (l.eval |@| r.eval){ applyBinaryNumericOp(_ * _) _ }
        case Divide(l, r)   =>
          (l.eval |@| r.eval){ applyBinaryNumericOp(_ / _) _ }
        case v @ Value(_)   =>
          v.right
      }

    def +(that: Expression): Expression =
      Plus(this, that)
    def -(that: Expression): Expression =
      Minus(this, that)
    def *(that: Expression): Expression =
      Multiply(this, that)
    def /(that: Expression): Expression =
      Divide(this, that)
    def ++(that: Expression): Expression =
      Append(this, that)
    def toUpper: Expression =
      UpperCase(this)
    def toLower: Expression =
      LowerCase(this)
  }
  object Expression {
    def applyBinaryNumericOp(op: (Double, Double) => Double)(l: Value, r: Value): Result =
      (l, r) match {
        case (Number(l), Number(r)) =>
          op(l, r).right
        case (Chars(l), _) =>
          errors.wrongValue("Number", "Chars", l)
        case (_, Chars(r)) =>
          errors.wrongValue("Number", "Chars", r)
      }

    object errors {
      def wrongValue(expected: String, received: String, value: String): Result =
        s"""
         | Expected value with tag $expected
         | but received value $value with tag $received
         """.trimMargin.left
    }

    // Smart constructor that creates Value with type Expression
    def value(in: Double): Expression =
      Value(in)
  }
  final case class Plus(left: Expression, right: Expression) extends Expression
  final case class Minus(left: Expression, right: Expression) extends Expression
  final case class Multiply(left: Expression, right: Expression) extends Expression
  final case class Divide(left: Expression, right: Expression) extends Expression
  final case class Append(left: Expression, right: Expression) extends Expression
  final case class UpperCase(string: Expression) extends Expression
  final case class LowerCase(string: Expression) extends Expression
  final case class Literal(get: Value) extends Expression

}
