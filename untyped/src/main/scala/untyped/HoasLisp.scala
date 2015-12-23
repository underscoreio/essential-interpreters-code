package untyped

import cats.data.Xor
import cats.syntax.apply._
import cats.syntax.xor._

object HoasLisp {
  sealed trait Value {
    val name: String =
      this match {
        case Number(_)   => "Number"
        case Chars(_)    => "Chars"
        case Bool(_)     => "Bool"
        case Function(_) => "Function"
      }
  }
  final case class Number(get: Double) extends Value
  final case class Chars(get: String) extends Value
  final case class Bool(get: Boolean) extends Value
  final case class Function(f: Value => Expression) extends Value

  sealed trait Expression {
    def apply(arg: Expression): Expression =
      Apply(this, arg)

    def apply(arg: Value): Expression =
      Apply(this, Literal(arg))
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
    implicit val boolRefine: ValueRefinement[Boolean] =
      make[Boolean]("Bool"){ case Bool(b) => b }
    implicit val functionRefine: ValueRefinement[Value => Expression] =
      make[Value => Expression]("Function"){ case Function(f) => f }
  }

  object Injections {
    import Injection._
    type ValueInjection[A] = Injection[A,Value]

    implicit val injectDouble: ValueInjection[Double] = Number.apply _
    implicit val injectString: ValueInjection[String] = Chars.apply _
    implicit val injectBoolean: ValueInjection[Boolean] = Bool.apply _
  }

  object Lift {
    import Expression._
    import Refinement._
    import Refinements._
    import Injection._
    import Injections._

    def lift[A : ValueRefinement, B : ValueInjection](f: A => B): Expression =
      fun { a =>
        Primitive(() => a.refine[A] map (a => f(a).inject[B]))
      }

    def lift[A : ValueRefinement, B : ValueRefinement, C : ValueInjection](f: (A, B) => C): Expression =
      fun { a =>
          fun { b =>
            Primitive(() => (a.refine[A] |@| b.refine[B]) map ((a,b) => f(a,b).inject[C]))
          }
      }
  }

  object Environment {
    import Lift._
    import Refinement._
    import Refinements._
    import Injection._
    import Injections._

    val + = lift[Double,Double,Double](_ + _)
    val - = lift[Double,Double,Double](_ - _)
    val * = lift[Double,Double,Double](_ * _)
    val / = lift[Double,Double,Double](_ / _)

    val < = lift[Double,Double,Boolean](_ < _)
    val > = lift[Double,Double,Boolean](_ > _)
    val == = lift[Double,Double,Boolean](_ == _)

    val `number->chars` = lift[Double,String](_.toString)

    val `chars-append` = lift[String,String,String](_ ++ _)
    val `chars-length` = lift[String,Double](_.length.toDouble)
  }

  object Interpreter {
    import Refinement._
    import Refinements._
    import Injection._
    import Injections._

    def eval(expr: Expression): Result[Value] =
      expr match {
        case Literal(v) => v.right
        case Apply(fExpr, argExpr) =>
          for {
            arg <- eval(argExpr)
            f   <- eval(fExpr) flatMap (v => v.refine[Value => Expression])
            v   <- eval(f(arg))
          } yield v
        case If(c, t, e) =>
          for {
            bool <- eval(c) flatMap (v => v.refine[Boolean])
            v    <- if(bool) eval(t) else eval(e)
          } yield v
        case Primitive(f) =>
          f()
      }
  }

  object Examples {
    import Environment._
    import Expression._
    import Interpreter._

    def square() = {
      val sq = fun(x => *(x)(x))
      val four = sq(number(2))
      val nine = sq(number(3))

      println(s"four is ${eval(four)}")
      println(s"nine is ${eval(nine)}")
    }

    def conditional() = {
      val four = perhaps(<(number(1))(number(2))){ number(4.0) }{ chars("bogus") }
      val bogus = perhaps(>(number(1))(number(2))){ number(4.0) }{ chars("bogus") }

      println(s"four is ${eval(four)}")
      println(s"bogus is ${eval(bogus)}")
    }
  }
}
