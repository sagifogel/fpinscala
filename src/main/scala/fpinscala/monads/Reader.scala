package fpinscala.monads

import scala.Function.const

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R]: Monad[({type f[x] = Reader[R, x]})#f] =
    new Monad[({type f[x] = Reader[R, x]})#f] {
      def unit[A](a: => A): Reader[R, A] = Reader(const(a))

      def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] =
        Reader(r => f(st.run(r)).run(r))
    }
}
