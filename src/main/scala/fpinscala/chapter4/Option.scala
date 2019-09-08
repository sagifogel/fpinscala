package fpinscala.chapter4

import fpinscala.datastructures.List
import fpinscala.datastructures.Cons
import fpinscala.datastructures.Nil

sealed trait Option[+A] {
  self =>
  def map[B](f: A => B): Option[B] = self match {
    case Some(value) => Some(f(value))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def getOrElse[B >: A](default: => B): B = self match {
    case Some(value) => value
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some.apply).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    if (map(f).getOrElse(false)) self
    else None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  import fpinscala.datastructures.List._

  def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A, B) => C): Option[C] = {
    fa.flatMap(a => fb.map(f(a, _)))
  }

  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    val zero: Option[List[A]] = Some(Nil)

    foldRight(as, zero)(map2(_, _)(Cons.apply))
  }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    val zero: Option[List[B]] = Some(Nil)

    foldRight(as, zero)((a, b) => map2(f(a), b)(Cons.apply))
  }

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(identity)
}
