package untyped

import org.scalacheck._

object ArithmeticSpec extends Properties("Arithmetic") {
  import Prop.forAll
  import Arithmetic._

  val genLiteral: Gen[(Expression, Double)] =
    Gen.chooseNum(1, 100) map { v => Literal(v) -> v }

  def genSubexpression(constructor: (Expression, Expression) => Expression)(f: (Double, Double) => Double): Int => Gen[(Expression, Double)] =
    (depth: Int) => {
      for {
        (e1, v1) <- genExpressionAndValue(depth - 1)
        (e2, v2) <- genExpressionAndValue(depth - 1)
      } yield constructor(e1, e2) -> f(v1, v2)
    }

  val genPlus = genSubexpression(Plus.apply _){ (a,b) => a + b }
  val genMinus = genSubexpression(Minus.apply _){ (a,b) => a - b }
  val genMultiply = genSubexpression(Multiply.apply _){ (a,b) => a * b }
  val genDivide = genSubexpression(Divide.apply _){ (a,b) => a / b }

  /** Generate both an expression and the value it evaluates to */
  def genExpressionAndValue(depth: Int): Gen[(Expression, Double)] =
    if(depth == 0)
      genLiteral
    else
      Gen.oneOf(genPlus(depth), genMinus(depth), genMultiply(depth), genDivide(depth))

  def gen: Gen[(Expression, Double)] =
    for {
      depth <- Gen.choose(0, 4)
      v     <- genExpressionAndValue(depth)
    } yield v

  property("eval") = forAll(gen){ case (e: Expression, v: Double) =>
    e.eval == v
  }
}
