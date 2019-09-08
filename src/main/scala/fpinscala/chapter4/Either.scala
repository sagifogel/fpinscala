package fpinscala.chapter4

import fpinscala.datastructures.{Cons, List, Nil}

sealed trait Either[+E, +A] {
  self =>
  def map[B](f: A => B): Either[E, B] =
    flatMap(Right[B] _ compose f)

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = self match {
    case l@Left(_) => l
    case Right(value) => f(value)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = self match {
    case Left(_) => b
    case r@Right(_) => r
  }

  def map2[EE >: E, B, C](fb: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    flatMap(a => fb.map(f(a, _)))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  import fpinscala.datastructures.List._

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    val zero: Either[E, List[B]] = Right(Nil)

    foldRight(as, zero)((a, b) => f(a).map2(b)(Cons.apply[B]))
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)
}
