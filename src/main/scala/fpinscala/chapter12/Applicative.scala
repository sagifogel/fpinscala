package fpinscala.chapter12

import fpinscala.chapter11.Functor
import fpinscala.chapter6.State

trait Applicative[F[_]] extends Functor[F] {
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_.apply(_))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def map2ViaApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    apply(apply(unit(f.curried))(fa))(fb)
  }

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  }

  def map3Second[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    map2(map2(fa, fb)((a, b) => (c: C) => f(a, b, c)), fc)(_.apply(_))
  }

  def map3Third[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    map2(product(fa, fb), fc) { case ((a, b), c) => f(a, b, c) }
  }

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    apply(map3(fa, fb, fc)((a, b, c) => (d: D) => f(a, b, c, d)))(fd)
  }

  def map4Second[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = {
    val zero = unit(Map.empty[K, V])
    ofa.foldRight(zero){ case ((k, fa), fb) => map2(fa, fb)((v, map) => map.updated(k, v)) }
  }
}

object Applicative {
  def validationApplicative[E] = new Applicative[({type f[a] = Validation[E, a]})#f] {
    override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = {
      (fa, fb) match {
        case (Success(a), Success(b)) => Success(f(a, b))
        case (Failure(head, tail), Failure(head2, tail2)) => Failure(head, (tail :+ head2) ++ tail2)
        case (_, f@Failure(_, _)) => f
        case (f@Failure(_, _), _) => f
      }
    }

    override def unit[A](a: => A): Validation[E, A] = Success(a)
  }

  def product[F[_], G[_]](F: Applicative[F], G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
        (F.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
      }

      override def unit[A](a: => A): (F[A], G[A]) = (F.unit(a), G.unit(a))
    }

  def compose[F[_], G[_]](F: Applicative[F], G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] = {
        F.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))
      }

      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
    }

  def stateMonad[S]: Monad[({type state[x] = State[S, x]})#state]
  = new Monad[({type state[x] = State[S, x]})#state] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
      ma flatMap f
  }
}
