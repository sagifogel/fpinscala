package fpinscala.chapter11

import fpinscala.chapter6.State
import fpinscala.chapter7.Par
import fpinscala.chapter7.Par.Par

import scala.Function.const
import scala.annotation.tailrec

case class Id[A](value: A)

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def flatMapViaMapAndJoin[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    join(map(ma)(f))
  }

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def map3[A, B, C, D](ma: F[A], mb: F[B], mc: F[C])(f: (A, B, C) => D): F[D] =
    map2(product(ma, mb), mc) { case ((a, b), c) => f(a, b, c) }

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    traverse(lma)(identity)
  }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B]))((a, fb) => map2(f(a), fb)(_ :: _))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
    traverse(List.range(0, n))(const(ma))
  }

  def replicateM2[A](n: Int, ma: F[A]): F[List[A]] = {
    @tailrec
    def go(list: List[Int], ma: F[A], listF: F[List[A]]): F[List[A]] = list match {
      case _ :: xs => go(xs, ma, map2(ma, listF)(_ :: _))
      case Nil => listF
    }

    go(List.range(0, n), ma, unit(List.empty))
  }

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    map(traverse(ms)(a => product(f(a), unit(a))))(_.collect { case (b, a) if b => a })
  }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def flatMapCompose[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    compose[F[A], A, B](identity, f)(ma)
  }

  def flatMapCompose2[A, B](ma: F[A])(f: A => F[B]): F[B] = {
    compose[Unit, A, B](const(ma), f)(())
  }

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)
}

object Monad {
  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = a #:: Stream.empty

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = a :: Nil

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)

    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma.value)
  }

  def stateMonad[S]: Monad[({type state[x] = State[S, x]})#state]
  = new Monad[({type state[x] = State[S, x]})#state] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
      ma flatMap f
  }

  def eitherMonad[T]: Monad[({type either[a] = Either[T, a]})#either] =
    new Monad[({type either[a] = Either[T, a]})#either] {
      override def unit[A](a: => A): Either[T, A] = Right(a)

      override def flatMap[A, B](ma: Either[T, A])(f: A => Either[T, B]): Either[T, B] = ma flatMap f
    }
}
