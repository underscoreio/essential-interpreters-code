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

    def make[A](name: String)(f: PartialFunction[Value,A]): ValueRefinement[A] = {
      val lifted = f.lift
      (v: Value) => lifted(v).fold(Errors.wrongTag[A](v, name))(a => a.right)
    }

    implicit val doubleRefine: ValueRefinement[Double] =
      make[Double]("Number"){ case Number(d) => d }
    implicit val stringRefine: ValueRefinement[String] =
      make[String]("Chars"){ case Chars(s) => s }
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

    def apply[A: ValueRefinement, B: ValueInjection](f: A => B)(a: Value): Result[Value] =
      (a.refine[A] map (a => f(a).inject[B]))

    def apply[A: ValueRefinement, B: ValueRefinement, C: ValueInjection](f: (A, B) => C)(a: Value, b: Value): Result[Value] =
      (a.refine[A] |@| b.refine[B]) map ((a,b) => f(a,b).inject[C])
  }

  sealed trait Expression {
    import Expression._
    import Injections._
    import Refinements._

    def eval: Result[Value] =
      this match {
        case Plus(l, r)     =>
          (l.eval |@| r.eval).tupled flatMap {
            (Lift.apply[Double,Double,Double]{ _ + _ } _).tupled
          }
        case Minus(l, r)    =>
          (l.eval |@| r.eval).tupled flatMap {
            (Lift.apply[Double,Double,Double]{ _ - _ } _).tupled
          }
        case Multiply(l, r) =>
          (l.eval |@| r.eval).tupled flatMap {
            (Lift.apply[Double,Double,Double]{ _ * _ } _).tupled
          }
        case Divide(l, r)   =>
          (l.eval |@| r.eval).tupled flatMap {
            (Lift.apply[Double,Double,Double]{ _ / _ } _).tupled
          }

        case Append(l, r)   =>
          (l.eval |@| r.eval).tupled flatMap {
            (Lift.apply[String,String,String]{ _ ++ _ } _).tupled
          }

        case UpperCase(s)   =>
          s.eval flatMap { Lift.apply[String,String]{ _.toUpperCase } _ }
        case LowerCase(s)   =>
          s.eval flatMap { Lift.apply[String,String]{ _.toLowerCase } _ }

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
