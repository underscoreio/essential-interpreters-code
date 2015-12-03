package untyped

import cats.data.Xor
import cats.syntax.apply._
import cats.syntax.xor._

object ALisp {
  type Environment = Map[Id,Value]

  final case class Id(get: String)

  sealed trait Value
  final case class Number(get: Double) extends Value
  final case class Chars(get: String) extends Value
  final case class Bool(get: Boolean) extends Value
  final case class Function(arg: Id, body: Expression, env: Environment) extends Value

  sealed trait Expression {
    def apply(arg: Expression): Expression =
      Apply(this, arg)
  }
  final case class Literal(get: Value) extends Expression
  final case class Ref(get: Id) extends Expression
  final case class Apply(f: Expression, arg: Expression) extends Expression
  final case class If(cond: Expression, thence: Expression, otherwise: Expression) extends Expression
  final case class PrimAp1(name: String, ref: Ref, f: Value => Result[Value]) extends Expression
  final case class PrimAp2(name: String, ref1: Ref, ref2: Ref, f: (Value, Value) => Result[Value]) extends Expression

  object Errors {
    def wrongTag[A](expected: String, received: String, value: String): Result[A] =
      s"""|Expected value with tag $expected
          |but received value $value with tag $received""".stripMargin.left

    def unboundId[A](id: Id, env: Environment): Result[A] =
      s"""|There is no binding for the name ${id.get}
          |in the environment $env""".stripMargin.left
  }

  object Checks {
    import Errors._

    def checkNumber(in: Value): Result[Double] =
      in match {
        case Number(v) => v.right
        case Chars(s) => wrongTag("Number", "Chars", s)
        case Bool(b) => wrongTag("Number", "Bool", b.toString)
        case Function(a, b, e) => wrongTag("Number", "Function", s"lambda($a){$b}")
      }
    def checkChars(in: Value): Result[String] =
      in match {
        case Chars(s) => s.right
        case Number(v) => wrongTag("Chars", "Number", v.toString)
        case Bool(b) => wrongTag("Chars", "Bool", b.toString)
        case Function(a, b, e) => wrongTag("Chars", "Function", s"lambda($a){$b}")
      }
    def checkFunction(in: Value): Result[Function] =
      in match {
        case f @ Function(a, b, e) => f.right
        case Chars(s) => wrongTag("Function", "Chars", s)
        case Number(v) => wrongTag("Function", "Number", v.toString)
        case Bool(b) => wrongTag("Function", "Bool", b.toString)
      }
    def checkBool(in: Value): Result[Boolean] =
      in match {
        case Bool(b) => b.right
        case Chars(s) => wrongTag("Bool", "Chars", s)
        case Number(v) => wrongTag("Bool", "Number", v.toString)
        case Function(a, b, e) => wrongTag("Bool", "Function", s"lambda($a){$b}")
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

    def lift[A : Refinement, B : Injection](name: String, f: A => B): Value =
      Function(
        Id("a"),
        PrimAp1(name, Ref(Id("a")), (a: Value) => (a.refine[A] map (a => f(a).inject))),
        Environment.empty
      )

    def lift[A : Refinement, B : Refinement, C : Injection](name: String, f: (A, B) => C): Value =
      Function(
        Id("a"),
        Literal(
          Function(
            Id("b"),
            PrimAp2(name, Ref(Id("a")), Ref(Id("b")), (a: Value, b: Value) =>
              (a.refine[A] |@| b.refine[B]) map ((a,b) => f(a,b).inject)
            ),
            Environment.empty
          )
        ),
        Environment.empty
      )
  }

  object Environment {
    import Lift._
    import Infrastructure._

    val empty: Environment =
      Map.empty

    def primitive1[A: Refinement, B: Injection](name: String)(f: A => B) =
      Id(name) -> lift(name, f)
    def primitive2[A: Refinement,B: Refinement,C: Injection](name: String)(f: (A,B) => C) =
      Id(name) -> lift(name, f)

    val initial: Environment =
      Map(
        primitive2("+"){ (a: Double, b: Double) => a + b },
        primitive2("*"){ (a: Double, b: Double) => a * b },
        primitive2("-"){ (a: Double, b: Double) => a - b },
        primitive2("/"){ (a: Double, b: Double) => a / b },
        primitive2("<"){ (a: Double, b: Double) => a < b },
        primitive2(">"){ (a: Double, b: Double) => a > b },
        primitive2("=="){ (a: Double, b: Double) => a == b },
        primitive1("number->chars"){ (a: Double) => a.toString },

        primitive2("chars-append"){ (a: String, b: String) => a ++ b },
        primitive1("chars-length"){ (a: String) => a.length.toDouble }
      )

    def lookup(env: Environment, id: Id): Result[Value] =
      env.get(id).fold[Result[Value]](Errors.unboundId(id, env -- (initial.keys))){ v => v.right }
  }

  object Expression {

    // Smart constructors ----------

    def number(in: Double): Expression =
      Literal(Number(in))
    def chars(in: String): Expression =
      Literal(Chars(in))
    def fun(arg: String)(body: Expression): Expression =
      Literal(Function(Id(arg), body, Environment.empty))
    def let(binding: (String, Expression))(body: Expression): Expression = {
      val (name, value) = binding
      Apply(Literal(Function(Id(name), body, Environment.empty)), value)
    }
    def perhaps(cond: Expression)(thence: Expression)(otherwise: Expression): Expression =
      If(cond, thence, otherwise)
    def ref(id: String): Expression =
      Ref(Id(id))
    val t = Bool(true)
    val f = Bool(false)
  }

  object Interpreter {
    import Environment._
    import Checks._

    def eval(expr: Expression, env: Environment = Environment.initial): Result[Value] =
      expr match {
        case Literal(v) =>
          v match {
            case Function(a, b, e) => Function(a, b, env ++ e).right
            case other => other.right
          }
        case Ref(id) =>
          lookup(env, id) flatMap { v =>
            v match {
              case Function(a, b, e) => Function(a, b, env ++ e).right
              case other => other.right
            }
          }
        case Apply(fExpr, argExpr) =>
          for {
            arg <- eval(argExpr, env)
            f   <- eval(fExpr, env) flatMap (checkFunction _)
            v   <- eval(f.body, env ++ f.env + (f.arg -> arg))
          } yield v
        case If(c, t, e) =>
          for {
            bool <- eval(c, env) flatMap (checkBool _)
            v    <- if(bool) eval(t, env) else eval(e, env)
          } yield v
        case PrimAp1(n, r, f) =>
          lookup(env, r.get) flatMap { v => f(v) }
        case PrimAp2(n, r1, r2, f) =>
          for {
            v1 <- lookup(env, r1.get)
            v2 <- lookup(env, r2.get)
            r  <- f(v1, v2)
          } yield r
      }
  }

  object Examples {
    import Expression._
    import Interpreter._

    def square() = {
      // This is how we write x => x*x
      val sq = fun("x"){ ref("*")(ref("x"))(ref("x")) }
      val four = sq(number(2))
      val nine = sq(number(3))

      println(s"four is ${eval(four)}")
      println(s"nine is ${eval(nine)}")
    }

    def aliasing() = {
      val sq = fun("x"){ ref("*")(ref("x"))(ref("x")) }

      // Ensure that values are bound correctly.
      //
      // If they are done incorrectly we'll alias x, a, and b, and calculate the
      // wrong answer.
      val four =
        let("x" -> number(4)){
          let("a" -> number(5)){
            let("b" -> number(6)){
              sq(number(2))
            }
          }
        }
      println(s"four is ${eval(four)}")
    }

    def conditional() = {
      val four = perhaps(ref("<")(number(1))(number(2))){ number(4.0) }{ chars("bogus") }
      val bogus = perhaps(ref(">")(number(1))(number(2))){ number(4.0) }{ chars("bogus") }

      println(s"four is ${eval(four)}")
      println(s"bogus is ${eval(bogus)}")
    }
  }
}
