package untyped

import cats.data.Xor

trait Infrastructure[Value] {
  type Refinement[A] = Value => Result[A]
  type Injection[A] = A => Value

  object syntax {
    implicit class RefinementOps(in: Value) {
      def refine[A](implicit r: Refinement[A]): Result[A] =
        r(in)
    }

    implicit class InjectionOps[A](in: A) {
      def inject(implicit i: Injection[A]): Value =
        i(in)
    }
  }
}
