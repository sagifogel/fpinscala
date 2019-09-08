package fpinscala.chapter6

import scala.Function.const

case class State[S, +A](run: S => (A, S)) {

  import State._

  def map[B](f: A => B): State[S, B] = {
    flatMap(unit[S, B] _ compose f)
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B] { s =>
    val (a, s2) = run(s)

    f(a).run(s2)
  }
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] = {
    ra.flatMap(a => rb.map(f(a, _)))
  }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    traverse(fs)(identity)
  }

  def traverse[S, A, B](fs: List[A])(f: A => State[S, B]): State[S, List[B]] = {
    val zero = unit[S, List[B]](List.empty)

    fs.foldRight(zero)((a, b) => map2(f(a), b)(_ :: _))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(const((), s))
}

