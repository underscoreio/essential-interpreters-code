package untyped

import cats.data.Xor
import cats.syntax.apply._
import cats.syntax.xor._

object ArithmeticAndStrings {
  type Result[A] = Xor[String,A]

  sealed trait Value {
    val name: String =
      this match {
        case Number(_) => "Number"
        case Chars(_) => "Chars"
      }
  }
  final case class Number(get: Double) extends Value
  final case class Chars(get: String) extends Value

  object Errors {
    def wrongTag[A](received: Value, expected: String): Result[A] =
      s"""|Expected value with tag $expected
          |but received value $received with tag ${received.name}""".stripMargin.left
  }

  object Refinements {
    import Refinement._
    type ValueRefinement[A] = Refinement[Value,A]

    implicit object doubleRefine extends ValueRefinement[Double] {
      def apply(in: Value): Result[Double] = {
        in match {
          case Number(d) => d.right
          case v         => (Errors.wrongTag(v, "Number"))
        }
      }
    }
    implicit object stringRefine extends ValueRefinement[String] {
      def apply(in: Value): Result[String] = {
        in match {
          case Chars(s) => s.right
          case v        => (Errors.wrongTag(v, "Chars"))
        }
      }
    }
  }

  object Injections {
    import Injection._
    type ValueInjection[A] = Injection[A,Value]

    implicit val injectDouble: ValueInjection[Double] = Number.apply _
    implicit val injectString: ValueInjection[String] = Chars.apply _
  }

  object Lift {
    import Expression._
    import Refinement._
    import Refinements._
    import Injection._
    import Injections._

    def apply[A: ValueRefinement, B: ValueInjection](a: Value)(f: A => B): Result[Value] =
      (a.refine[A] map (a => f(a).inject[B]))

    def apply[A: ValueRefinement, B: ValueRefinement, C: ValueInjection](a: Value, b: Value)(f: (A, B) => C): Result[Value] =
      (a.refine[A] |@| b.refine[B]) map ((a,b) => f(a,b).inject[C])
  }

  sealed trait Expression {
    import Expression._
    import Injections._
    import Refinements._

    def eval: Result[Value] =
      this match {
        case Plus(l, r)     =>
          (l.eval |@| r.eval).tupled flatMap { case (a,b) =>
            Lift.apply[Double,Double,Double](a,b){ _ + _ }
          }
        case Minus(l, r)    =>
          (l.eval |@| r.eval).tupled flatMap { case (a,b) =>
            Lift.apply[Double,Double,Double](a,b){ _ - _ }
          }
        case Multiply(l, r) =>
          (l.eval |@| r.eval).tupled flatMap { case (a,b) =>
            Lift.apply[Double,Double,Double](a,b){ _ * _ }
          }
        case Divide(l, r)   =>
          (l.eval |@| r.eval).tupled flatMap { case (a,b) =>
            Lift.apply[Double,Double,Double](a,b){ _ / _ }
          }

        case Append(l, r)   =>
          (l.eval |@| r.eval).tupled flatMap { case (a,b) =>
            Lift.apply[String,String,String](a,b){ _ ++ _ }
          }

        case UpperCase(s)   =>
          s.eval flatMap { a => Lift.apply[String,String](a){ _.toUpperCase } }
        case LowerCase(s)   =>
          s.eval flatMap { a => Lift.apply[String,String](a){ _.toLowerCase } }

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
