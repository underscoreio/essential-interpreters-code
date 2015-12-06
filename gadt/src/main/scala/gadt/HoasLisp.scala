package gadt

object HoasLisp {
  sealed trait Expression[A] 
  final case class Literal[A](get: A) extends Expression[A]
  final case class Apply[A,B](f: Expression[A => B], arg: Expression[A]) extends Expression[B]
  final case class If[A](cond: Expression[Boolean], thence: Expression[A], otherwise: Expression[A]) extends Expression[A]
  final case class Function[A,B](f: A => Expression[B]) extends Expression[A => B]
  object Expression {
    def literal[A](in: A): Expression[A] =
      Literal(in)
    def fun[A,B](f: A => Expression[B]): Expression[A => B] =
      Function(f)
    def perhaps[A](cond: Expression[Boolean])(thence: Expression[A])(otherwise: Expression[A]): Expression[A] =
      If(cond, thence, otherwise)
  }

  object Lift {
    import Expression._

    def lift[A,B](f: A => B): Expression[A => B] =
      Function(a => Literal(f(a)))

    def lift[A,B,C](f: (A,B) => C): Expression[A => B => C] =
      Function(a => Function(b => Literal(f(a,b))))
  }

  object Environment {
    import Lift._

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
    def eval[A](expr: Expression[A]): A =
      expr match {
        case Literal(v) => v
        case a: Apply[a,b] =>
          val arg = eval(a.arg)
          val f   = eval(a.f)
          f(arg)
        case If(c,t,e) =>
          val b = eval(c)
          if(b) eval(t) else eval(e)
        case f: Function[a,b] =>
          (a: a) => eval(f.f(a))
      }
  }

  object Syntax {
    implicit class FunctionOps[A,B](f: Expression[A => B]) {
      def apply(arg: Expression[A]): Expression[B] =
        Apply(f, arg)

      def apply(arg: A): Expression[B] =
        Apply(f, Literal(arg))
    }

  }

  object Examples {
    import Environment._
    import Expression._
    import Interpreter._
    import Syntax._

    def square() = {
      val sq = fun[Double,Double](x => *(x)(x))
      val four = sq(2.0)
      val nine = sq(3.0)

      println(s"four is ${eval(four)}")
      println(s"nine is ${eval(nine)}")
    }

    def conditional() = {
      val four = perhaps(<(literal(1.0))(literal(2.0))){ literal(4.0) }{ literal(9.0) }
      val nine = perhaps(>(literal(1.0))(literal(2.0))){ literal(4.0) }{ literal(9.0) }

      println(s"four is ${eval(four)}")
      println(s"nine is ${eval(nine)}")
    }
  }
}
