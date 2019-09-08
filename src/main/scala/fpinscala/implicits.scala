package fpinscala

object Syntax {
  implicit class TupleOps[A, B](val pair: (A, B)) extends AnyVal {
    def first: A = pair._1
    def second: B = pair._2
  }

  implicit class Function2Ops[A, B, C](val f: (A, B) => C) extends AnyVal {
    def flip: (B, A) => C = (b , a) => f(a, b)
  }
}
