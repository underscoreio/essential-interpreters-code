package object untyped {
  import cats.data.Xor

  type Result[A] = Xor[String,A]
}
