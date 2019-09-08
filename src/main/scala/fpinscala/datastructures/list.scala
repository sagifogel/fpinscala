package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Cons(_, xs) => xs
    case _ => Nil
  }

  def setHead[A](a: A, as: List[A]): List[A] = {
    Cons(a, tail(as))
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @tailrec
    def go(i: Int, as: List[A]): List[A] = {
      if (i <= 0) as
      else go(i - 1, as match {
        case Cons(_, tail) => tail
        case _ => Nil
      })
    }

    go(n, l)
  }

  def length[A](as: List[A]): Int = {
    @tailrec
    def go(count: Int, l: List[A]): Int = l match {
      case Cons(_, tail) => go(count + 1, tail)
      case _ => count
    }

    go(0, as)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    @tailrec
    def go(i: Int, as: List[A]): List[A] = {
      if (i <= 0) as
      else go(i - 1, as match {
        case Cons(head, tail) if f(head) => tail
        case _ => as
      })
    }

    go(length(l), l)
  }

  def tailViaDrop[A](as: List[A]): List[A] = drop(as, 1)

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case Nil => l
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      case Nil => z
    }
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      case Nil => z
    }
  }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def length2[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))
  }

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, identity[B] _)((a, fn) => (b: B) => fn(f(b, a)))(z)
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons.apply)
  }

  def flatten[A](a1: List[List[A]]): List[A] = {
    foldRight(a1, Nil: List[A])(append)
  }

  def addOne(list: List[Int]): List[Int] = {
    foldRight(list, Nil: List[Int])((h, t) => Cons(h + 1, t))
  }

  def double2String(list: List[Double]): List[String] = {
    foldRight(list, Nil: List[String])((h, t) => Cons(h.toString, t))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    flatten(map(as)(f))
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)
  }

  def zipInts(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Cons(h, t), Cons(h2, t2)) => Cons(h + h2, zipInts(t, t2))
    case (ls, Nil) => ls
    case (Nil, rs) => rs
    case _ => Nil
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Cons(h, t), Cons(h2, t2)) => Cons(f(h, h2), zipWith(t, t2)(f))
    case _ => Nil
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sub match {
    case Cons(x, _) =>
      val dropped = dropWhile(sup)(_ != x)

      if (length(dropped) == 0) false
      else foldRight(zipWith(dropped, sub)(_ == _), true)(_ && _)
    case Nil => true
  }
}