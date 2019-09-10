package fpinscala.syntax

object Syntax {
  implicit class Function2Ops[A, B, C](val f: (A, B) => C) extends AnyVal {
    def flip: (B, A) => C = (b , a) => f(a, b)
  }
}
