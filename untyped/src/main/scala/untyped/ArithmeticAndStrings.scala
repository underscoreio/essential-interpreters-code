package untyped

import cats.data.Xor
import cats.syntax.apply._
import cats.syntax.xor._

object ArithmeticAndStrings {
  type Result[A] = Xor[String,A]

  sealed trait Value
  final case class Number(get: Double) extends Value
  final case class Chars(get: String) extends Value

  sealed trait Expression {
    import Expression._

    def eval: Result[Value] =
      this match {
        case Plus(l, r)     =>
          binaryNumericOp(l, r){ _ + _ }
        case Minus(l, r)    =>
          binaryNumericOp(l, r){ _ - _ }
        case Multiply(l, r) =>
          binaryNumericOp(l, r){ _ * _ }
        case Divide(l, r)   =>
          binaryNumericOp(l, r){ _ / _ }

        case Append(l, r)   =>
          binaryCharsOp(l, r){ _ ++ _ }

        case UpperCase(s)   =>
          unaryStringOp(s){ _.toUpperCase }
        case LowerCase(s)   =>
          unaryStringOp(s){ _.toLowerCase }

        case Literal(v)   =>
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
    def checkNumber(in: Value): Result[Double] =
      in match {
        case Number(v) => v.right
        case Chars(s)  => errors.wrongTag("Number", "Chars", s)
      }
    def checkChars(in: Value): Result[String] =
      in match {
        case Chars(s)  => s.right
        case Number(v) => errors.wrongTag("Chars", "Number", v.toString)
      }

    def binaryNumericOp(l: Expression, r: Expression)(op: (Double, Double) => Double): Result[Value] =
      ((l.eval flatMap checkNumber) |@| (r.eval flatMap checkNumber)) map { (x,y) => Number(op(x, y)) }
    def binaryCharsOp(l: Expression, r: Expression)(op: (String, String) => String): Result[Value] =
      ((l.eval flatMap checkChars) |@| (r.eval flatMap checkChars)) map { (x,y) => Chars(op(x, y)) }

    def unaryStringOp(v: Expression)(op: String => String): Result[Value] =
      v.eval flatMap checkChars map { x => Chars(op(x)) }

    object errors {
      def wrongTag[A](expected: String, received: String, value: String): Result[A] =
        s"""|Expected value with tag $expected
            |but received value $value with tag $received""".stripMargin.left
    }

    // Smart constructors -----------

    def number(in: Double): Expression =
      Literal(Number(in))
    def chars(in: String): Expression =
      Literal(Chars(in))
  }
  final case class Plus(left: Expression, right: Expression) extends Expression
  final case class Minus(left: Expression, right: Expression) extends Expression
  final case class Multiply(left: Expression, right: Expression) extends Expression
  final case class Divide(left: Expression, right: Expression) extends Expression
  final case class Append(left: Expression, right: Expression) extends Expression
  final case class UpperCase(string: Expression) extends Expression
  final case class LowerCase(string: Expression) extends Expression
  final case class Literal(get: Value) extends Expression

  object Example {
    def go() = {
      import Expression._

      val expr1 = number(1.0) + number(3.0) / number(2.0)
      val expr2 = chars("Hello, ") ++ chars("world!")
      val expr3 = (number(1.0) * number(4.0)).toUpper

      println(expr1.eval)
      println(expr2.eval)
      println(expr3.eval)
    }
  }

}
