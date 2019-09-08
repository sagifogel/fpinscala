package fpinscala.chapter12

import fpinscala.chapter11.Functor

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {
  val treeTraversable: Traverse[Tree] = new Traverse[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      Tree(f(fa.head), fa.tail.map(map(_)(f)))

    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = {
      ???
    }
  }
}
