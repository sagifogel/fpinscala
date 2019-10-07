package fpinscala.applicative

import fpinscala.state.State

trait Monad[F[_]] extends Applicative[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(f(a, _)))
}

object Monad {
  def stateMonad[S]: Monad[({type state[x] = State[S, x]})#state]
  = new Monad[({type state[x] = State[S, x]})#state] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] =
      ma flatMap f
  }

  def composeM[F[_], G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] = {
    new Monad[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))

      override def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] = {
        F.flatMap(fa)(ga => {
          F.map(T.sequence(G.map(ga)(f))(F))(G.join[B])
        })
      }
    }
  }

}
