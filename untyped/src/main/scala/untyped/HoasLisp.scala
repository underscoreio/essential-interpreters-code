package untyped

import cats.data.Xor
import cats.syntax.apply._
import cats.syntax.xor._

object HoasLisp {
  sealed trait Value
  final case class Number(get: Double) extends Value
  final case class Chars(get: String) extends Value
  final case class Bool(get: Boolean) extends Value
  final case class Function(f: Value => Expression) extends Value

  sealed trait Expression {
    def apply(arg: Expression): Expression =
      Apply(this, arg)
  }
  final case class Literal(get: Value) extends Expression
  final case class Apply(f: Expression, arg: Expression) extends Expression
  final case class If(cond: Expression, thence: Expression, otherwise: Expression) extends Expression
  final case class Primitive(f: () => Result[Value]) extends Expression
  object Expression {

    // Smart constructors ----------

    def literal(in: Value): Expression =
      Literal(in)
    def number(in: Double): Expression =
      Literal(Number(in))
    def chars(in: String): Expression =
      Literal(Chars(in))
    def fun(f: Value => Expression): Expression =
      Literal(Function(f))
    def perhaps(cond: Expression)(thence: Expression)(otherwise: Expression): Expression =
      If(cond, thence, otherwise)
    val t = Bool(true)
    val f = Bool(false)
  }

  object Errors {
    def wrongTag[A](expected: String, received: String, value: String): Result[A] =
      s"""|Expected value with tag $expected
          |but received value $value with tag $received""".stripMargin.left
  }

  object Checks {
    import Errors._

    def name(in: Value): String =
      in match {
        case Number(_)   => "Number"
        case Chars(_)    => "Chars"
        case Bool(_)     => "Bool"
        case Function(_) => "Function"
      }

    def checkNumber(in: Value): Result[Double] =
      in match {
        case Number(v) => v.right
        case other     => wrongTag("Number", name(other), other.toString)
      }
    def checkChars(in: Value): Result[String] =
      in match {
        case Chars(s) => s.right
        case other    => wrongTag("Chars", name(other), other.toString)
      }
    def checkFunction(in: Value): Result[Value => Expression] =
      in match {
        case Function(f) => f.right
        case other       => wrongTag("Function", name(other), other.toString)
      }
    def checkBool(in: Value): Result[Boolean] =
      in match {
        case Bool(b) => b.right
        case other   => wrongTag("Bool", name(other), other.toString)
      }
  }
  object Infrastructure extends Infrastructure[Value] {
    import Checks._
    implicit val refineDouble: Refinement[Double] = checkNumber _
    implicit val refineString: Refinement[String] = checkChars _
    implicit val refineBoolean: Refinement[Boolean] = checkBool _

    import Expression._
    implicit val injectDouble: Injection[Double] = Number.apply _
    implicit val injectString: Injection[String] = Chars.apply _
    implicit val injectBoolean: Injection[Boolean] = Bool.apply _
  }

  object Lift {
    import Infrastructure._
    import Infrastructure.syntax._
    import Expression._

    def lift[A : Refinement, B : Injection](f: A => B): Value =
      Function { a =>
        Primitive(() => a.refine[A] map (a => f(a).inject))
      }

    def lift[A : Refinement, B : Refinement, C : Injection](f: (A, B) => C): Value =
      Function { a =>
        Literal(
          Function { b =>
            Primitive(() => (a.refine[A] |@| b.refine[B]) map ((a,b) => f(a,b).inject))
          }
        )
      }
  }

  object Environment {
    import Infrastructure._
    import Lift._

    val + = lift[Double,Double,Double](_ + _)
    val - = lift[Double,Double,Double](_ - _)
  }

  object Interpreter {
    import Checks._
    import Infrastructure._

    def eval(expr: Expression): Result[Value] =
      expr match {
        case Literal(v) => v.right
        case Apply(fExpr, argExpr) =>
          for {
            arg <- eval(argExpr)
            f   <- eval(fExpr) flatMap (checkFunction _)
            v   <- eval(f(arg))
          } yield v
        case If(c, t, e) =>
          for {
            bool <- eval(c) flatMap (checkBool _)
            v    <- if(bool) eval(t) else eval(e)
          } yield v
        case Primitive(f) =>
          f()
      }
  }
}
