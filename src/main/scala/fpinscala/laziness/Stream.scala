package fpinscala.laziness

import scala.annotation.tailrec
import scala.Function.const
import fpinscala.errorhandling.Option._
import fpinscala.errorhandling.Option
import fpinscala.errorhandling.Some
import fpinscala.errorhandling.None

sealed trait Stream[+A] {
  self =>

  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = {
    @tailrec
    def go(list: List[A], xs: Stream[A]): List[A] = xs match {
      case Cons(h, t) => go(h() :: list, t())
      case Empty => list
    }

    go(Nil, self).reverse
  }

  def take(n: Int): Stream[A] = self match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = self match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => self
  }

  def takeWhile(p: A => Boolean): Stream[A] = self match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => b && p(a))
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)
  }

  def headOption2: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, b) => cons(f(a), b))
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)(cons(_, _))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a, b) => f(a).append(b))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)
  }

  def map2[B](f: A => B): Stream[B] =
    unfold(self) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def take2(n: Int): Stream[A] =
    unfold((self, n)) {
      case (Cons(h, t), i) if i > 0 => Some((h(), (t(), i - 1)))
      case _ => None
    }

  def takeWhile3(p: A => Boolean): Stream[A] = {
    unfold(self) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }
  }

  def zipWith[B, C](as: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((self, as)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((self, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case _ => None
    }

  def tails: Stream[Stream[A]] =
    unfold(self) {
      case s@Cons(_, t) => Some((s, t()))
      case _ => None
    } append Stream(empty)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    tails.flatMap(stream => {
      Stream(stream.foldRight(z)(f))
    })
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  val ones: Stream[Int] = constant(1)

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def fib(n: Int): Stream[Int] = {
    @tailrec
    def go(current: Int, next: Int, j: Int, stream: Stream[Int]): Stream[Int] = {
      if (j == 0) stream
      else go(next, current + next, j - 1, stream.append(cons(next, empty)))
    }

    go(0, 1, n, empty)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

  def fib2: Stream[Int] = {
    unfold((0, 1)) { case (current, next) =>
      Some((next, (next, current + next)))
    }
  }

  def from2(n: Int): Stream[Int] = {
    unfold(n)(i => Some((i, i + 1)))
  }

  def constant2[A](a: A): Stream[A] = unfold(a)(const(Some((a, a))))
}
